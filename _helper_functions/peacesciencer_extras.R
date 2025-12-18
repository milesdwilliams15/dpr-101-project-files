##########################################################
# Add MIC and political relevance variables to a dataset #
##########################################################


# add_icd_mics() ----------------------------------------------------------

add_icd_mics <- function(
    data, level
) {
  # check for CoW system codes:
  yes <- any(colnames(data) %in% c("ccode", "ccode1"))
  if(!yes) {
    stop("You can only use this function with CoW system codes.")
  }
  
  # whittle MIE data to MIC level:
  dt <- suppressMessages(
    read_csv(
      "https://raw.githubusercontent.com/milesdwilliams15/foreign-figures/refs/heads/main/_data/mie-1.0.csv"
    )
  )
  
  dt |>
    group_by(
      ccode1, ccode2, styear, micnum
    ) |>
    summarize(
      init = max((eventnum == 1), na.rm = T),
      across(fatalmin1:fatalmax2, sum),
      hostlev = max(hostlev),
      .groups = "drop"
    ) |> 
    group_by(micnum) |>
    mutate(
      icdmiconset = (min(styear) == styear) + 0,
      hostlev = max(hostlev)
    ) -> dt
  
  # only keep those of certain hostlev:
  if(!missing(level)) {
    dt |>
      filter(hostlev >= level) -> dt
  }
  
  # some more aggregating:
  dt |>
    group_by(ccode1, ccode2, styear, micnum) |>
    summarize(
      icdmicongoing = 1,
      icdmiconset = max(icdmiconset),
      icdmicongoing_init1 = max(init),
      icdmiconset_init1 = max(init) * icdmiconset,
      across(fatalmin1:fatalmax2, sum),
      .groups = "drop"
    ) -> dt
  
  # the data isn't mirrored:
  bind_rows(
    dt,
    dt |>
      rename(
        ccode2 = ccode1,
        ccode1 = ccode2,
        icdmicongoing_init2 = icdmicongoing_init1,
        icdmiconset_init2 = icdmiconset_init1,
        fatalmin2 = fatalmin1,
        fatalmin1 = fatalmin2,
        fatalmax2 = fatalmax1,
        fatalmax1 = fatalmax2
      )
  ) |> 
    rename(year = styear) |>
    mutate(
      across(everything(), ~ replace_na(.x, 0))
    ) |> 
    select(
      ccode1, 
      ccode2, 
      year, 
      starts_with("gml_"), 
      everything()
    ) -> dt
  
  # if the data is dyadic:
  if(any(colnames(data) %in% c("ccode1"))) {
    data |>
      left_join(
        dt, by = c("ccode1", "ccode2", "year")
      ) -> dt
    
    # fill in missings:
    dt |>
      mutate(
        across(
          starts_with("icdmic") | starts_with("fatal"),
          ~ replace_na(.x, 0)
        )
      ) -> dt
  } else {
    dt |>
      rename(ccode = ccode1) |>
      group_by(ccode, year) |>
      summarize(
        icdmicongoing = 1,
        icdmiconset = max(icdmiconset),
        icdmicongoing_init = max(icdmicongoing_init1),
        icdmiconset_init = max(icdmiconset_init1),
        fatalmin = sum(fatalmin1),
        fatalmax = sum(fatalmax1),
        fatalmin_total = sum(fatalmin1 + fatalmin2),
        fatalmax_total = sum(fatalmax1 + fatalmax2),
        .groups = "drop"
      ) -> dt
    
    # merge:
    data |>
      left_join(
        dt,
        by = c("ccode", "year")
      ) -> dt
    
    # fill missings:
    dt |>
      mutate(
        across(
          starts_with("icdmic") | starts_with("fatal"),
          ~ replace_na(.x, 0)
        )
      ) -> dt
  }
  
  # return
  dt
}


# add_relevance() ---------------------------------------------------------

add_relevance <- function(data) {
  create_dyadyears() |>
    add_cow_majors() |>
    add_contiguity() |>
    add_capital_distance() -> ddy
  
  logit <- function(x) 1 / (1 + exp(-x))
  
  ddy |>
    mutate(
      contig = ifelse(conttype >= 1, 1, 0),
      majdyad = pmax(cowmaj1, cowmaj2)
    ) |>
    transmute(
      ccode1, ccode2, year,
      prd1 = ifelse(contig == 1 | majdyad == 1, 1, 0),
      prd2 = logit(
        4.801 + 4.50*contig - 1.051*log(capdist) + 2.901*majdyad)
    ) -> ddy
  
  if(any(colnames(data) == "ccode")) {
    ddy |>
      group_by(ccode1, year) |>
      summarize(
        prd1 = sum(prd1, na.rm = T),
        prd2 = sum(prd2, na.rm = T),
        .groups = "drop"
      ) |>
      rename(
        ccode = ccode1
      ) -> cy
    
    left_join(
      data, cy, by = c("ccode", "year")
    )
  } else {
    left_join(data, ddy, by = c("ccode1", "ccode2", "year"))
  }
}


# add_int_order ------------------------------------------------------

add_int_order <- function(data) {
  # Use code from:
  # https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/Q90FOB/KBGKHK&version=1.0
  # This was code used by Braumoeller to code Int'l Orders
  #
  # Add codings from Andy Goodhart for 19th and 20th century international orders
  # Very generous definition of international order to avoid missing any
  # Also codes new states that arose from the disintegration of order
  
  # Format of variable names is c1.xxx and c2.xxx for c1, c2. xxx's are:
  # LatinAm: Latin American Decolonization from 1810-1858
  # concert: Concert of Europe from 1816-1852
  # concert.gp: Concert of Europe, Great Powers only
  # interim: period btw Concert and Bismarck
  # bismarck: Bismarckian order
  # IndependentAfterWWI: Newly independent states after WWI
  # MandateAfterWWI: Middle East territories under int'l mandate (coded by countries that emerged)
  # League: League of Nations
  # PostWarLiberal: Post-WWII liberal international order
  # PostWarCommunist: Post-WWII communist international order
  # PostWarOther: Third World and nonaligned middle powers
  
  # Latin American Decolonization from 1810-1858. Note that 1858 is 30 years after the last declaration of independence in this period (i.e., Uruguay in 1828).
  
  data$c1.LatinAm<-0
  data$c1.LatinAm[data$ccode1==160 & data$year>1815 & data$year<1857]<- 1 # Argentina declared independence in 1816.
  data$c1.LatinAm[data$ccode1==145 & data$year>1824 & data$year<1857]<- 1 # Bolivia gained indepdendence in 1825.
  data$c1.LatinAm[data$ccode1==140 & data$year>1821 & data$year<1857]<- 1 # Brazil gained independence in 1822.
  data$c1.LatinAm[data$ccode1==155 & data$year>1809 & data$year<1857]<- 1 # Chile broke from Spanish rule in 1810 and the royalist forces collapsed in 1826.
  data$c1.LatinAm[data$ccode1==100 & data$year>1819 & data$year<1857]<- 1 # Colombia gained independence from the Spanish in 1820.
  data$c1.LatinAm[data$ccode1==94 & data$year>1820 & data$year<1857]<- 1 # Costa Rica gained independence in 1821.
  data$c1.LatinAm[data$ccode1==130 & data$year>1821 & data$year<1857]<- 1 # Ecuador gained independence in 1822.
  data$c1.LatinAm[data$ccode1==92 & data$year>1820 & data$year<1857]<- 1 # El Salvador gained independence in 1821.
  data$c1.LatinAm[data$ccode1==90 & data$year>1820 & data$year<1857]<- 1 # Guatemala gained independence in 1821.
  data$c1.LatinAm[data$ccode1==91 & data$year>1820 & data$year<1857]<- 1 # Honduras gained independence in 1821.
  data$c1.LatinAm[data$ccode1==70 & data$year>1820 & data$year<1857]<- 1 # Mexico gained independence in 1821.
  data$c1.LatinAm[data$ccode1==93 & data$year>1820 & data$year<1857]<- 1 # Nicaragua gained independence in 1821 but control of the country was not yet consolidated.
  data$c1.LatinAm[data$ccode1==95 & data$year>1820 & data$year<1857]<- 1 # Panama gained independence in 1821.
  data$c1.LatinAm[data$ccode1==135 & data$year>1820 & data$year<1857]<- 1 # Peru declared independence in 1821 and defeated the Spanish in 1824.
  data$c1.LatinAm[data$ccode1==150 & data$year>1810 & data$year<1857]<- 1 # Paraguay gained independence in 1811.
  data$c1.LatinAm[data$ccode1==165 & data$year>1827 & data$year<1857]<- 1 # Uruguay gained independence in 1828.
  data$c1.LatinAm[data$ccode1==101 & data$year>1820 & data$year<1857]<- 1 # Venezuela gained independence in 1821.
  
  data$c2.LatinAm<-0
  data$c2.LatinAm[data$ccode2==160 & data$year>1815 & data$year<1857]<- 1 # Argentina declared independence in 1816.
  data$c2.LatinAm[data$ccode2==145 & data$year>1824 & data$year<1857]<- 1 # Bolivia gained indepdendence in 1825.
  data$c2.LatinAm[data$ccode2==140 & data$year>1821 & data$year<1857]<- 1 # Brazil gained independence in 1822.
  data$c2.LatinAm[data$ccode2==155 & data$year>1809 & data$year<1857]<- 1 # Chile broke from Spanish rule in 1810 and the royalist forces collapsed in 1826.
  data$c2.LatinAm[data$ccode2==100 & data$year>1819 & data$year<1857]<- 1 # Colombia gained independence from the Spanish in 1820.
  data$c2.LatinAm[data$ccode2==94 & data$year>1820 & data$year<1857]<- 1 # Costa Rica gained independence in 1821.
  data$c2.LatinAm[data$ccode2==130 & data$year>1821 & data$year<1857]<- 1 # Ecuador gained independence in 1822.
  data$c2.LatinAm[data$ccode2==92 & data$year>1820 & data$year<1857]<- 1 # El Salvador gained independence in 1821.
  data$c2.LatinAm[data$ccode2==90 & data$year>1820 & data$year<1857]<- 1 # Guatemala gained independence in 1821.
  data$c2.LatinAm[data$ccode2==91 & data$year>1820 & data$year<1857]<- 1 # Honduras gained independence in 1821.
  data$c2.LatinAm[data$ccode2==70 & data$year>1820 & data$year<1857]<- 1 # Mexico gained independence in 1821.
  data$c2.LatinAm[data$ccode2==93 & data$year>1820 & data$year<1857]<- 1 # Nicaragua gained independence in 1821 but control of the country was not yet consolidated.
  data$c2.LatinAm[data$ccode2==95 & data$year>1820 & data$year<1857]<- 1 # Panama gained independence in 1821.
  data$c2.LatinAm[data$ccode2==135 & data$year>1820 & data$year<1857]<- 1 # Peru declared independence in 1821 and defeated the Spanish in 1824.
  data$c2.LatinAm[data$ccode2==150 & data$year>1810 & data$year<1857]<- 1 # Paraguay gained independence in 1811.
  data$c2.LatinAm[data$ccode2==165 & data$year>1827 & data$year<1857]<- 1 # Uruguay gained independence in 1828.
  data$c2.LatinAm[data$ccode2==101 & data$year>1820 & data$year<1857]<- 1 # Venezuela gained independence in 1821.
  
  
  # Concert of Europe from 1816-1852: Includes all states in Europe (CCODES 199-399)
  
  data$c1.concert<-0
  data$c1.concert[data$ccode1>199 & data$ccode1<400 & data$year>1815 & data$year<1853]<- 1
  
  data$c2.concert<- 0
  data$c2.concert[data$ccode2>199 & data$ccode2<400 & data$year>1815 & data$year<1853]<- 1
  
  data$c1.concert.gp<-0
  data$c1.concert.gp[data$ccode1 %in% c(200,220,255,300,365) & data$year>1815 & data$year<1853]<- 1
  
  data$c2.concert.gp<- 0
  data$c2.concert.gp[data$ccode2 %in% c(200,220,255,300,365) & data$year>1815 & data$year<1853]<- 1
  
  
  # Bismarckian Order from 1855-1870: Includes all states in Europe (CCODES 199-399)
  
  data$c1.interim<-0
  data$c1.interim[data$ccode1>199 & data$ccode1<400 & data$year>1854 & data$year<1871]<- 1
  
  data$c2.interim<- 0
  data$c2.interim[data$ccode2>199 & data$ccode2<400 & data$year>1854 & data$year<1871]<- 1
  
  # Bismarckian Order from 1871-1890: Includes all states in Europe (CCODES 199-399)
  
  data$c1.bismarck<-0
  data$c1.bismarck[data$ccode1>199 & data$ccode1<400 & data$year>1870 & data$year<1891]<- 1
  
  data$c2.bismarck<- 0
  data$c2.bismarck[data$ccode2>199 & data$ccode2<400 & data$year>1870 & data$year<1891]<- 1
  
  
  
  # Wilhelmine System from 1890-1913
  
  data$c1.wilhelm<-0
  data$c1.wilhelm[data$ccode1>199 & data$ccode1<400 & data$year>1889 & data$year<1915]<- 1
  
  data$c2.wilhelm<- 0
  data$c2.wilhelm[data$ccode2>199 & data$ccode2<400 & data$year>1889 & data$year<1915]<- 1
  
  # Newly Independent States after the First World War (Source: https://en.wikipedia.org/wiki/Aftermath_of_World_War_I#New_nations_break_free; https://www.britannica.com/place/Yugoslavia-former-federated-nation-1929-2003;
  # https://www.loc.gov/law/help/us-treaties/bevans/m-ust000002-0043.pdf; and various pages in the CIA World Factbook and Encyclopedia Britannica online; https://history.state.gov/countries/egypt
  
  data$c1.IndependentAfterWWI<-0
  data$c1.IndependentAfterWWI[data$ccode1==700 & data$year>1918 & data$year<1939]<-1 # Afghanistan gained independence from Britain in 1919.
  data$c1.IndependentAfterWWI[data$ccode1==339 & data$year>1911 & data$year<1939]<-1 # Albania gained independence from the Ottomans in 1912.
  data$c1.IndependentAfterWWI[data$ccode1==371 & data$year>1917 & data$year<1921]<-1 # Armenia was briefly independent from 1918 to 1920.
  data$c1.IndependentAfterWWI[data$ccode1==305 & data$year>1917 & data$year<1939]<-1 # Austria emerged as an independent country in 1918 and remained so until the German Anschluss of 1938.
  data$c1.IndependentAfterWWI[data$ccode1==373 & data$year>1917 & data$year<1921]<-1 # Azerbaijan was briefly independent from 1918-1920 but had imperfect control over its territory. The Russians invaded in 1920.
  data$c1.IndependentAfterWWI[data$ccode1==355 & data$year>1907 & data$year<1939]<-1 # Bulgaria gained independence from the Ottomans in 1908.
  data$c1.IndependentAfterWWI[data$ccode1==315 & data$year>1917 & data$year<1940]<-1 # Czechoslovakia was established in 1918. Germany annexed part of it in 1938.
  data$c1.IndependentAfterWWI[data$ccode1==651 & data$year>1921 & data$year<1940]<-1 # Egypt gained indendepence in 1922.
  data$c1.IndependentAfterWWI[data$ccode1==366 & data$year>1917 & data$year<1940]<-1 # Estonia declared indendepence in 1918. 
  data$c1.IndependentAfterWWI[data$ccode1==375 & data$year>1916 & data$year<1940]<-1 # Finland gained independence in 1917.
  data$c1.IndependentAfterWWI[data$ccode1==372 & data$year>1917 & data$year<1922]<-1 # Georgia was briefly independent from 1918-1921. 
  data$c1.IndependentAfterWWI[data$ccode1==310 & data$year>1917 & data$year<1940]<-1 # Hungary emerged as an independent country in 1918.
  data$c1.IndependentAfterWWI[data$ccode1==205 & data$year>1920 & data$year<1940]<-1 # Ireland gained independence from Britain in 1921. 
  data$c1.IndependentAfterWWI[data$ccode1==367 & data$year>1917 & data$year<1940]<-1 # Latvia gained independence in 1918. 
  data$c1.IndependentAfterWWI[data$ccode1==368 & data$year>1917 & data$year<1940]<-1 # Lithuania gained independence in 1918.
  data$c1.IndependentAfterWWI[data$ccode1==290 & data$year>1917 & data$year<1940]<-1 # Poland gained independence in 1918.
  data$c1.IndependentAfterWWI[data$ccode1==670 & data$year>1931 & data$year<1939]<-1 # Saudi Arabia became independent and unified in 1932.
  data$c1.IndependentAfterWWI[data$ccode1==369 & data$year>1917 & data$year<1921]<-1 # Ukraine was briefly independent from 1918-1920. 
  data$c1.IndependentAfterWWI[data$ccode1==678 & data$year>1917 & data$year<1939]<-1 # Yemen (Yemen Arab Republic/North Yemen) gained independence from the Ottomans in 1918.
  data$c1.IndependentAfterWWI[data$ccode1==345 & data$year>1918 & data$year<1940]<-1 # Yugoslavia (known as Kingdom of Serbs, Croats, and Slovenes est. by Paris Peace Conf. in 1919 and became Yugoslavia in 1929)
  
  data$c2.IndependentAfterWWI<-0
  data$c2.IndependentAfterWWI[data$ccode2==700 & data$year>1918 & data$year<1939]<-1 # Afghanistan gained independence from Britain in 1919.
  data$c2.IndependentAfterWWI[data$ccode2==339 & data$year>1911 & data$year<1939]<-1 # Albania gained independence from the Ottomans in 1912.
  data$c2.IndependentAfterWWI[data$ccode2==371 & data$year>1917 & data$year<1921]<-1 # Armenia was briefly independent from 1918 to 1920.
  data$c2.IndependentAfterWWI[data$ccode2==305 & data$year>1917 & data$year<1939]<-1 # Austria emerged as an independent country in 1918 and remained so until the German Anschluss of 1938.
  data$c2.IndependentAfterWWI[data$ccode2==373 & data$year>1917 & data$year<1921]<-1 # Azerbaijan was briefly independent from 1918-1920 but had imperfect control over its territory. The Russians invaded in 1920.
  data$c2.IndependentAfterWWI[data$ccode2==355 & data$year>1907 & data$year<1939]<-1 # Bulgaria gained independence from the Ottomans in 1908.
  data$c2.IndependentAfterWWI[data$ccode2==315 & data$year>1917 & data$year<1940]<-1 # Czechoslovakia was established in 1918. Germany annexed part of it in 1938.
  data$c1.IndependentAfterWWI[data$ccode2==651 & data$year>1921 & data$year<1940]<-1 # Egypt gained indendepence in 1922.
  data$c2.IndependentAfterWWI[data$ccode2==366 & data$year>1917 & data$year<1940]<-1 # Estonia declared indendepence in 1918. 
  data$c2.IndependentAfterWWI[data$ccode2==375 & data$year>1916 & data$year<1940]<-1 # Finland gained independence in 1917.
  data$c2.IndependentAfterWWI[data$ccode2==372 & data$year>1917 & data$year<1922]<-1 # Georgia was briefly independent from 1918-1921. 
  data$c2.IndependentAfterWWI[data$ccode2==310 & data$year>1917 & data$year<1940]<-1 # Hungary emerged as an independent country in 1918.
  data$c2.IndependentAfterWWI[data$ccode2==205 & data$year>1920 & data$year<1940]<-1 # Ireland gained independence from Britain in 1921. 
  data$c2.IndependentAfterWWI[data$ccode2==367 & data$year>1917 & data$year<1940]<-1 # Latvia gained independence in 1918. 
  data$c2.IndependentAfterWWI[data$ccode2==368 & data$year>1917 & data$year<1940]<-1 # Lithuania gained independence in 1918.
  data$c2.IndependentAfterWWI[data$ccode2==290 & data$year>1917 & data$year<1940]<-1 # Poland gained independence in 1918.
  data$c2.IndependentAfterWWI[data$ccode2==670 & data$year>1931 & data$year<1939]<-1 # Saudi Arabia became independent and unified in 1932.
  data$c2.IndependentAfterWWI[data$ccode2==369 & data$year>1917 & data$year<1921]<-1 # Ukraine was briefly independent from 1918-1920. 
  data$c2.IndependentAfterWWI[data$ccode2==678 & data$year>1917 & data$year<1939]<-1 # Yemen (Yemen Arab Republic/North Yemen) gained independence from the Ottomans in 1918.
  data$c2.IndependentAfterWWI[data$ccode2==345 & data$year>1918 & data$year<1940]<-1 # Yugoslavia (known as Kingdom of Serbs, Croats, and Slovenes est. by Paris Peace Conf. in 1919 and became Yugoslavia in 1929)
  
  # Middle East territories under International Mandate, post-World War I (coded by the countries that emerged from those territories).  
  # Note that this section does not include Middle East territories under British control/influence (but not formal mandate) during this period. 
  # (UAE, Bahrain, Qatar, Kuwait had relationships with Britain that predate WWI.)
  # Present day Palestine (West Bank & Gaza) does not have a CCODE so it is absent here. 
  # Sourcs: https://www.britannica.com/place/Syria/The-French-mandate; https://history.state.gov/countries/syria; https://history.state.gov/countries/lebanon; 
  # https://en.wikipedia.org/wiki/Anglo-Iraqi_Treaty; https://en.wikipedia.org/wiki/British_Mandate_for_Mesopotamia_(legal_instrument);
  
  data$c1.MandateAfterWWI<-0
  data$c1.MandateAfterWWI[data$ccode1==652 & data$year>1921 & data$year<1940]<-1 # Syria: French Mandate started in 1922
  data$c1.MandateAfterWWI[data$ccode1==660 & data$year>1921 & data$year<1940]<-1 # Lebanon: French Mandate started in 1922
  data$c1.MandateAfterWWI[data$ccode1==645 & data$year>1921 & data$year<1940]<-1 # Iraq: British Mandate for Mesopotamia enacted 1922
  data$c1.MandateAfterWWI[data$ccode1==666 & data$year>1921 & data$year<1940]<-1 # Israel: British Mandate for Palestine enacted 1922
  data$c1.MandateAfterWWI[data$ccode1==663 & data$year>1921 & data$year<1940]<-1 # Jordan: British Mandate for Palestine enacted 1922
  
  data$c2.MandateAfterWWI<-0
  data$c2.MandateAfterWWI[data$ccode2==652 & data$year>1921 & data$year<1940]<-1 # Syria: French Mandate started in 1922
  data$c2.MandateAfterWWI[data$ccode2==660 & data$year>1921 & data$year<1940]<-1 # Lebanon: French Mandate started in 1922
  data$c2.MandateAfterWWI[data$ccode2==645 & data$year>1921 & data$year<1940]<-1 # Iraq: British Mandate for Mesopotamia enacted 1922
  data$c2.MandateAfterWWI[data$ccode2==666 & data$year>1921 & data$year<1940]<-1 # Israel: British Mandate for Palestine enacted 1922
  data$c2.MandateAfterWWI[data$ccode2==663 & data$year>1921 & data$year<1940]<-1 # Jordan: British Mandate for Palestine enacted 1922
  
  # Leage of Nations System from 1919-1939 (Source: http://www.indiana.edu/~league/nationalmember.htm, accessed 9 Feb 2017)
  
  data$c1.League<-0
  data$c1.League[data$ccode1==700 & data$year>1933 & data$year<1940]<- 1 # Afghanistan 1934-1939
  data$c1.League[data$ccode1==339 & data$year>1919 & data$year<1940]<- 1 # Albania 1920-1939(annexed by Italy April, 12 1939)
  data$c1.League[data$ccode1==160 & data$year>1919 & data$year<1940]<- 1 # Argentina 1920-1939
  data$c1.League[data$ccode1==900 & data$year>1919 & data$year<1940]<- 1 # Australia 1920-1939
  data$c1.League[data$ccode1==305 & data$year>1919 & data$year<1939]<- 1 # Austria 1920-1939 (annexed by Germany April 10, 1938)
  data$c1.League[data$ccode1==211 & data$year>1919 & data$year<1940]<- 1 # Belgium 1920-1939
  data$c1.League[data$ccode1==145 & data$year>1919 & data$year<1940]<- 1 # Bolivia 1920-1939
  data$c1.League[data$ccode1==140 & data$year>1919 & data$year<1927]<- 1 # Brazil 1920-1926
  data$c1.League[data$ccode1==200 & data$year>1919 & data$year<1940]<- 1 # United Kingdom 1920-1939 # NOTE THAT BRITISH EMPIRE WAS THE SAME -- NEED TO ADD THEM. 
  data$c1.League[data$ccode1==355 & data$year>1919 & data$year<1940]<- 1 # Bulgaria 1920-1939
  data$c1.League[data$ccode1==20 & data$year>1919 & data$year<1940]<- 1 # Canada 1920-1939
  data$c1.League[data$ccode1==155 & data$year>1919 & data$year<1939]<- 1 # Chile 1920-1938
  data$c1.League[data$ccode1==710 & data$year>1919 & data$year<1940]<- 1 # China 1920-1939
  data$c1.League[data$ccode1==100 & data$year>1919 & data$year<1940]<- 1 # Colombia 1920-1939
  data$c1.League[data$ccode1==94 & data$year>1919 & data$year<1926]<- 1 # Costa Rica 1920-1925
  data$c1.League[data$ccode1==40 & data$year>1919 & data$year<1940]<- 1 # Cuba 1920-1939
  data$c1.League[data$ccode1==315 & data$year>1919 & data$year<1940]<- 1 # Czechoslovakia 1920-1939 (annexed by Germany March 15, 1939)
  data$c1.League[data$ccode1==390 & data$year>1919 & data$year<1940]<- 1 # Denmark 1920-1939
  data$c1.League[data$ccode1==42 & data$year>1923 & data$year<1940]<- 1 # Dominican Republic 1924-1939
  data$c1.League[data$ccode1==130 & data$year>1933 & data$year<1940]<- 1 # Ecuador 1934-1939
  data$c1.League[data$ccode1==651 & data$year>1936 & data$year<1940]<- 1 # Egypt 1937-1939
  data$c1.League[data$ccode1==366 & data$year>1920 & data$year<1940]<- 1 # Estonia 1921-1939
  data$c1.League[data$ccode1==530 & data$year>1922 & data$year<1937]<- 1 # Ethiopia 1923-1936 (annexed by Italy May 9, 1936)
  data$c1.League[data$ccode1==375 & data$year>1919 & data$year<1940]<- 1 # Finland 1920-1939
  data$c1.League[data$ccode1==220 & data$year>1919 & data$year<1942]<- 1 # France 1920-1941 ### WHY WAS THIS LATER? DID THEY JUST NOT FORMALLY WITHDRAW UNTIL THEN?
  data$c1.League[data$ccode1==255 & data$year>1925 & data$year<1934]<- 1 # Germany 1926-1933
  data$c1.League[data$ccode1==350 & data$year>1919 & data$year<1940]<- 1 # Greece 1920-1939
  data$c1.League[data$ccode1==90 & data$year>1919 & data$year<1937]<- 1 # Guatemala 1920-1936
  data$c1.League[data$ccode1==41 & data$year>1919 & data$year<1940]<- 1 # Haiti 1920-1942 ### CHECK THIS END DATE
  data$c1.League[data$ccode1==91 & data$year>1919 & data$year<1937]<- 1 # Honduras 1920-1936
  data$c1.League[data$ccode1==310 & data$year>1921 & data$year<1940]<- 1 # Hungary 1922-1939
  data$c1.League[data$ccode1==750 & data$year>1919 & data$year<1940]<- 1 # India 1920-1939
  data$c1.League[data$ccode1==630 & data$year>1919 & data$year<1940]<- 1 # Iran 1920-1939
  data$c1.League[data$ccode1==645 & data$year>1931 & data$year<1940]<- 1 # Iraq 1932-1939
  data$c1.League[data$ccode1==205 & data$year>1920 & data$year<1940]<- 1 # Ireland gained independence from Britain in 1921.
  data$c1.League[data$ccode1==325 & data$year>1919 & data$year<1938]<- 1 # Italy 1920-1937
  data$c1.League[data$ccode1==740 & data$year>1919 & data$year<1934]<- 1 # Japan 1920-1933
  data$c1.League[data$ccode1==367 & data$year>1920 & data$year<1940]<- 1 # Latvia 1921-1939
  data$c1.League[data$ccode1==450 & data$year>1919 & data$year<1940]<- 1 # Liberia 1920-1939
  data$c1.League[data$ccode1==368 & data$year>1920 & data$year<1940]<- 1 # Lithuania 1921-1939
  data$c1.League[data$ccode1==212 & data$year>1919 & data$year<1940]<- 1 # Luxembourg 1920-1939
  data$c1.League[data$ccode1==70 & data$year>1930 & data$year<1940]<- 1 # Mexico 1931-1939
  data$c1.League[data$ccode1==210 & data$year>1919 & data$year<1940]<- 1 # Netherlands 1920-1939
  data$c1.League[data$ccode1==920 & data$year>1919 & data$year<1940]<- 1 # New Zealand 1920-1939
  data$c1.League[data$ccode1==93 & data$year>1919 & data$year<1937]<- 1 # Nicaragua 1920-1936
  data$c1.League[data$ccode1==385 & data$year>1919 & data$year<1940]<- 1 # Norway 1920-1939
  data$c1.League[data$ccode1==95 & data$year>1919 & data$year<1940]<- 1 # Panama 1920-1939
  data$c1.League[data$ccode1==150 & data$year>1919 & data$year<1936]<- 1 # Paraguay 1920-1935
  data$c1.League[data$ccode1==135 & data$year>1919 & data$year<1940]<- 1 # Peru 1920-1939
  data$c1.League[data$ccode1==290 & data$year>1919 & data$year<1940]<- 1 # Poland 1920-1939
  data$c1.League[data$ccode1==235 & data$year>1919 & data$year<1940]<- 1 # Portugal 1920-1939
  data$c1.League[data$ccode1==360 & data$year>1919 & data$year<1941]<- 1 # Romania 1920-1940 ### NOTE LATE DATE
  data$c1.League[data$ccode1==92 & data$year>1919 & data$year<1938]<- 1 # El Salvador 1920-1937
  data$c1.League[data$ccode1==560 & data$year>1919 & data$year<1940]<- 1 # South Africa 1920-1939
  data$c1.League[data$ccode1==230 & data$year>1919 & data$year<1940]<- 1 # Spain 1920-1939
  data$c1.League[data$ccode1==380 & data$year>1919 & data$year<1940]<- 1 # Sweden 1920-1939
  data$c1.League[data$ccode1==225 & data$year>1919 & data$year<1940]<- 1 # Switzerland 1920-1939
  data$c1.League[data$ccode1==800 & data$year>1919 & data$year<1940]<- 1 # Thailand 1920-1939
  data$c1.League[data$ccode1==640 & data$year>1931 & data$year<1940]<- 1 # Turkey 1932-1939
  data$c1.League[data$ccode1==365 & data$year>1933 & data$year<1940]<- 1 # Russia for USSR 1934-1939 ### HOW TO HANDLE USSR? NO CCODE - JUST RUSSIA.
  data$c1.League[data$ccode1==165 & data$year>1919 & data$year<1940]<- 1 # Uruguay 1920-1939
  data$c1.League[data$ccode1==101 & data$year>1919 & data$year<1939]<- 1 # Venezuela 1920-1938
  data$c1.League[data$ccode1==345 & data$year>1919 & data$year<1940]<- 1 # Yugoslavia 1920-1939
  
  data$c2.League<- 0
  data$c2.League[data$ccode2==700 & data$year>1933 & data$year<1940]<- 1 # Afghanistan 1934-1939
  data$c2.League[data$ccode2==339 & data$year>1919 & data$year<1940]<- 1 # Albania 1920-1939(annexed by Italy April, 12 1939)
  data$c2.League[data$ccode2==160 & data$year>1919 & data$year<1940]<- 1 # Argentina 1920-1939
  data$c2.League[data$ccode2==900 & data$year>1919 & data$year<1940]<- 1 # Australia 1920-1939
  data$c2.League[data$ccode2==305 & data$year>1919 & data$year<1939]<- 1 # Austria 1920-1939 (annexed by Germany April 10, 1938)
  data$c2.League[data$ccode2==211 & data$year>1919 & data$year<1940]<- 1 # Belgium 1920-1939
  data$c2.League[data$ccode2==145 & data$year>1919 & data$year<1940]<- 1 # Bolivia 1920-1939
  data$c2.League[data$ccode2==140 & data$year>1919 & data$year<1927]<- 1 # Brazil 1920-1926
  data$c2.League[data$ccode2==200 & data$year>1919 & data$year<1940]<- 1 # United Kingdom 1920-1939 # NOTE THAT BRITISH EMPIRE WAS THE SAME -- NEED TO ADD THEM. 
  data$c2.League[data$ccode2==355 & data$year>1919 & data$year<1940]<- 1 # Bulgaria 1920-1939
  data$c2.League[data$ccode2==20 & data$year>1919 & data$year<1940]<- 1 # Canada 1920-1939
  data$c2.League[data$ccode2==155 & data$year>1919 & data$year<1939]<- 1 # Chile 1920-1938
  data$c2.League[data$ccode2==710 & data$year>1919 & data$year<1940]<- 1 # China 1920-1939
  data$c2.League[data$ccode2==100 & data$year>1919 & data$year<1940]<- 1 # Colombia 1920-1939
  data$c2.League[data$ccode2==94 & data$year>1919 & data$year<1926]<- 1 # Costa Rica 1920-1925
  data$c2.League[data$ccode2==40 & data$year>1919 & data$year<1940]<- 1 # Cuba 1920-1939
  data$c2.League[data$ccode2==315 & data$year>1919 & data$year<1940]<- 1 # Czechoslovakia 1920-1939 (annexed by Germany March 15, 1939)
  data$c2.League[data$ccode2==390 & data$year>1919 & data$year<1940]<- 1 # Denmark 1920-1939
  data$c2.League[data$ccode2==42 & data$year>1923 & data$year<1940]<- 1 # Dominican Republic 1924-1939
  data$c2.League[data$ccode2==130 & data$year>1933 & data$year<1940]<- 1 # Ecuador 1934-1939
  data$c2.League[data$ccode2==651 & data$year>1936 & data$year<1940]<- 1 # Egypt 1937-1939
  data$c2.League[data$ccode2==366 & data$year>1920 & data$year<1940]<- 1 # Estonia 1921-1939
  data$c2.League[data$ccode2==530 & data$year>1922 & data$year<1937]<- 1 # Ethiopia 1923-1936 (annexed by Italy May 9, 1936)
  data$c2.League[data$ccode2==375 & data$year>1919 & data$year<1940]<- 1 # Finland 1920-1939
  data$c2.League[data$ccode2==220 & data$year>1919 & data$year<1942]<- 1 # France 1920-1941 ### WHY WAS THIS LATER? DID THEY JUST NOT FORMALLY WITHDRAW UNTIL THEN?
  data$c2.League[data$ccode2==255 & data$year>1925 & data$year<1934]<- 1 # Germany 1926-1933
  data$c2.League[data$ccode2==350 & data$year>1919 & data$year<1940]<- 1 # Greece 1920-1939
  data$c2.League[data$ccode2==90 & data$year>1919 & data$year<1937]<- 1 # Guatemala 1920-1936
  data$c2.League[data$ccode2==41 & data$year>1919 & data$year<1940]<- 1 # Haiti 1920-1942 ### CHECK THIS END DATE
  data$c2.League[data$ccode2==91 & data$year>1919 & data$year<1937]<- 1 # Honduras 1920-1936
  data$c2.League[data$ccode2==310 & data$year>1921 & data$year<1940]<- 1 # Hungary 1922-1939
  data$c2.League[data$ccode2==750 & data$year>1919 & data$year<1940]<- 1 # India 1920-1939
  data$c2.League[data$ccode2==630 & data$year>1919 & data$year<1940]<- 1 # Iran 1920-1939
  data$c2.League[data$ccode2==645 & data$year>1931 & data$year<1940]<- 1 # Iraq 1932-1939
  data$c2.League[data$ccode2==205 & data$year>1920 & data$year<1940]<- 1 # Ireland gained independence from Britain in 1921.
  data$c2.League[data$ccode2==325 & data$year>1919 & data$year<1938]<- 1 # Italy 1920-1937
  data$c2.League[data$ccode2==740 & data$year>1919 & data$year<1934]<- 1 # Japan 1920-1933
  data$c2.League[data$ccode2==367 & data$year>1920 & data$year<1940]<- 1 # Latvia 1921-1939
  data$c2.League[data$ccode2==450 & data$year>1919 & data$year<1940]<- 1 # Liberia 1920-1939
  data$c2.League[data$ccode2==368 & data$year>1920 & data$year<1940]<- 1 # Lithuania 1921-1939
  data$c2.League[data$ccode2==212 & data$year>1919 & data$year<1940]<- 1 # Luxembourg 1920-1939
  data$c2.League[data$ccode2==70 & data$year>1930 & data$year<1940]<- 1 # Mexico 1931-1939
  data$c2.League[data$ccode2==210 & data$year>1919 & data$year<1940]<- 1 # Netherlands 1920-1939
  data$c2.League[data$ccode2==920 & data$year>1919 & data$year<1940]<- 1 # New Zealand 1920-1939
  data$c2.League[data$ccode2==93 & data$year>1919 & data$year<1937]<- 1 # Nicaragua 1920-1936
  data$c2.League[data$ccode2==385 & data$year>1919 & data$year<1940]<- 1 # Norway 1920-1939
  data$c2.League[data$ccode2==95 & data$year>1919 & data$year<1940]<- 1 # Panama 1920-1939
  data$c2.League[data$ccode2==150 & data$year>1919 & data$year<1936]<- 1 # Paraguay 1920-1935
  data$c2.League[data$ccode2==135 & data$year>1919 & data$year<1940]<- 1 # Peru 1920-1939
  data$c2.League[data$ccode2==290 & data$year>1919 & data$year<1940]<- 1 # Poland 1920-1939
  data$c2.League[data$ccode2==235 & data$year>1919 & data$year<1940]<- 1 # Portugal 1920-1939
  data$c2.League[data$ccode2==360 & data$year>1919 & data$year<1941]<- 1 # Romania 1920-1940 ### NOTE LATE DATE
  data$c2.League[data$ccode2==92 & data$year>1919 & data$year<1938]<- 1 # El Salvador 1920-1937
  data$c2.League[data$ccode2==560 & data$year>1919 & data$year<1940]<- 1 # South Africa 1920-1939
  data$c2.League[data$ccode2==230 & data$year>1919 & data$year<1940]<- 1 # Spain 1920-1939
  data$c2.League[data$ccode2==380 & data$year>1919 & data$year<1940]<- 1 # Sweden 1920-1939
  data$c2.League[data$ccode2==225 & data$year>1919 & data$year<1940]<- 1 # Switzerland 1920-1939
  data$c2.League[data$ccode2==800 & data$year>1919 & data$year<1940]<- 1 # Thailand 1920-1939
  data$c2.League[data$ccode2==640 & data$year>1931 & data$year<1940]<- 1 # Turkey 1932-1939
  data$c2.League[data$ccode2==365 & data$year>1933 & data$year<1940]<- 1 # Russia for USSR 1934-1939 ### HOW TO HANDLE USSR? NO CCODE - JUST RUSSIA.
  data$c2.League[data$ccode2==165 & data$year>1919 & data$year<1940]<- 1 # Uruguay 1920-1939
  data$c2.League[data$ccode2==101 & data$year>1919 & data$year<1939]<- 1 # Venezuela 1920-1938
  data$c2.League[data$ccode2==345 & data$year>1919 & data$year<1940]<- 1 # Yugoslavia 1920-1939
  
  # Post World War II Liberal Order from 1945 - 1991 (i.e., "First World" aligned against the Soviet Union) see: http://www.nato.int/cps/en/natohq/topics_52044.htm
  
  # Below are NATO Members through 1991; states aligned with the West but not in NATO; and neutral states that are liberal, democratic. 
  
  data$c1.PostWarLiberal<-0
  data$c1.PostWarLiberal[data$ccode1==211 & data$year>1944 & data$year<1992]<- 1 # Belgium
  data$c1.PostWarLiberal[data$ccode1==20 & data$year>1944 & data$year<1992]<- 1 # Canada
  data$c1.PostWarLiberal[data$ccode1==390 & data$year>1944 & data$year<1992]<- 1 # Denmark
  data$c1.PostWarLiberal[data$ccode1==220 & data$year>1944 & data$year<1992]<- 1 # France
  data$c1.PostWarLiberal[data$ccode1==395 & data$year>1944 & data$year<1992]<- 1 # Iceland
  data$c1.PostWarLiberal[data$ccode1==325 & data$year>1944 & data$year<1992]<- 1 # Italy
  data$c1.PostWarLiberal[data$ccode1==212 & data$year>1944 & data$year<1992]<- 1 # Luxembourg
  data$c1.PostWarLiberal[data$ccode1==210 & data$year>1944 & data$year<1992]<- 1 # Netherlands
  data$c1.PostWarLiberal[data$ccode1==385 & data$year>1944 & data$year<1992]<- 1 # Norway
  data$c1.PostWarLiberal[data$ccode1==235 & data$year>1944 & data$year<1992]<- 1 # Portugal
  data$c1.PostWarLiberal[data$ccode1==200 & data$year>1944 & data$year<1992]<- 1 # United Kingdom
  data$c1.PostWarLiberal[data$ccode1==2 & data$year>1944 & data$year<1992]<- 1 # United States
  data$c1.PostWarLiberal[data$ccode1==350 & data$year>1951 & data$year<1992]<- 1 # Greece joined NATO in 1952
  data$c1.PostWarLiberal[data$ccode1==640 & data$year>1951 & data$year<1992]<- 1 # Turkey joined NATO in 1952
  data$c1.PostWarLiberal[data$ccode1==260 & data$year>1954 & data$year<1991]<- 1 # German Federal Republic joined NATO in 1955 and was a member through October 1990
  data$c1.PostWarLiberal[data$ccode1==255 & data$year>1989 & data$year<1992]<- 1 # Unified Germany joined NATO in October 1990 ## note overlap with W. Germany in 1990.
  data$c1.PostWarLiberal[data$ccode1==230 & data$year>1981 & data$year<1992]<- 1 # Spain joined NATO in 1982
  data$c1.PostWarLiberal[data$ccode1==666 & data$year>1947 & data$year<1992]<- 1 # Israel founded in 1948
  data$c1.PostWarLiberal[data$ccode1==740 & data$year>1951 & data$year<1992]<- 1 # Japan: US occupation ended in 1952. See: http://afe.easia.columbia.edu/special/japan_1900_occupation.htm
  data$c1.PostWarLiberal[data$ccode1==732 & data$year>1947 & data$year<1992]<- 1 # Republic of Korea. US occupation ended in 1948. Note CCODE 732 is the ROK specific code. 
  data$c1.PostWarLiberal[data$ccode1==900 & data$year>1944 & data$year<1992]<- 1 # Australia. Note that the ANZUS treaty started in 1951 but Australia was aligned with West prior. 
  data$c1.PostWarLiberal[data$ccode1==920 & data$year>1944 & data$year<1992]<- 1 # New Zealand. Note that the ANZUS treaty started in 1951 but NZ was aligned with West prior. 
  data$c1.PostWarLiberal[data$ccode1==305 & data$year>1944 & data$year<1992]<- 1 # Austria was western, liberal democratic but neutral. Note CCODE 305 is the Austria-specific code. There is a separate Austria-Hungary code (300)
  data$c1.PostWarLiberal[data$ccode1==205 & data$year>1944 & data$year<1992]<- 1 # Ireland was western, liberal democratic but neutral.
  data$c1.PostWarLiberal[data$ccode1==380 & data$year>1944 & data$year<1992]<- 1 # Sweden was western, liberal democratic but neutral.
  data$c1.PostWarLiberal[data$ccode1==225 & data$year>1944 & data$year<1992]<- 1 # Switzerland was western, liberal democratic but neutral.
  data$c1.PostWarLiberal[data$ccode1==817 & data$year>1954 & data$year<1976]<- 1 # Republic of ("South") Vietnam as of 1955.
  data$c1.PostWarLiberal[data$ccode1==713 & data$year>1946 & data$year<1992]<- 1 # Republic of China ("Taiwan") concluded its consitution in 1947. 
  
  
  data$c2.PostWarLiberal<-0
  data$c2.PostWarLiberal[data$ccode2==211 & data$year>1944 & data$year<1992]<- 1 # Belgium
  data$c2.PostWarLiberal[data$ccode2==20 & data$year>1944 & data$year<1992]<- 1 # Canada
  data$c2.PostWarLiberal[data$ccode2==390 & data$year>1944 & data$year<1992]<- 1 # Denmark
  data$c2.PostWarLiberal[data$ccode2==220 & data$year>1944 & data$year<1992]<- 1 # France
  data$c2.PostWarLiberal[data$ccode2==395 & data$year>1944 & data$year<1992]<- 1 # Iceland
  data$c2.PostWarLiberal[data$ccode2==325 & data$year>1944 & data$year<1992]<- 1 # Italy
  data$c2.PostWarLiberal[data$ccode2==212 & data$year>1944 & data$year<1992]<- 1 # Luxembourg
  data$c2.PostWarLiberal[data$ccode2==210 & data$year>1944 & data$year<1992]<- 1 # Netherlands
  data$c2.PostWarLiberal[data$ccode2==385 & data$year>1944 & data$year<1992]<- 1 # Norway
  data$c2.PostWarLiberal[data$ccode2==235 & data$year>1944 & data$year<1992]<- 1 # Portugal
  data$c2.PostWarLiberal[data$ccode2==200 & data$year>1944 & data$year<1992]<- 1 # United Kingdom
  data$c2.PostWarLiberal[data$ccode2==2 & data$year>1944 & data$year<1992]<- 1 # United States
  data$c2.PostWarLiberal[data$ccode2==350 & data$year>1951 & data$year<1992]<- 1 # Greece joined NATO in 1952
  data$c2.PostWarLiberal[data$ccode2==640 & data$year>1951 & data$year<1992]<- 1 # Turkey joined NATO in 1952
  data$c2.PostWarLiberal[data$ccode2==260 & data$year>1954 & data$year<1991]<- 1 # German Federal Republic joined NATO in 1955 and was a member through October 1990
  data$c2.PostWarLiberal[data$ccode2==255 & data$year>1989 & data$year<1992]<- 1 # Unified Germany joined NATO in October 1990 ## note overlap with W. Germany in 1990.
  data$c2.PostWarLiberal[data$ccode2==230 & data$year>1981 & data$year<1992]<- 1 # Spain joined NATO in 1982
  data$c2.PostWarLiberal[data$ccode2==666 & data$year>1947 & data$year<1992]<- 1 # Israel founded in 1948
  data$c2.PostWarLiberal[data$ccode2==740 & data$year>1951 & data$year<1992]<- 1 # Japan: US occupation ended in 1952. See: http://afe.easia.columbia.edu/special/japan_1900_occupation.htm
  data$c2.PostWarLiberal[data$ccode2==732 & data$year>1947 & data$year<1992]<- 1 # Republic of Korea. US occupation ended in 1948. Note CCODE 732 is the ROK specific code.
  data$c2.PostWarLiberal[data$ccode2==900 & data$year>1944 & data$year<1992]<- 1 # Australia. Note that the ANZUS treaty started in 1951 but Australia was aligned with West prior. 
  data$c2.PostWarLiberal[data$ccode2==920 & data$year>1944 & data$year<1992]<- 1 # New Zealand. Note that the ANZUS treaty started in 1951 but NZ was aligned with West prior. 
  data$c2.PostWarLiberal[data$ccode2==305 & data$year>1944 & data$year<1992]<- 1 # Austria was western, liberal democratic but neutral. Note CCODE 305 is the Austria-specific code. There is a separate Austria-Hungary code (300)
  data$c2.PostWarLiberal[data$ccode2==205 & data$year>1944 & data$year<1992]<- 1 # Ireland was western, liberal democratic but neutral.
  data$c2.PostWarLiberal[data$ccode2==380 & data$year>1944 & data$year<1992]<- 1 # Sweden was western, liberal democratic but neutral.
  data$c2.PostWarLiberal[data$ccode2==225 & data$year>1944 & data$year<1992]<- 1 # Switzerland was western, liberal democratic but neutral.
  data$c2.PostWarLiberal[data$ccode2==817 & data$year>1954 & data$year<1976]<- 1 # Republic of ("South") Vietnam as of 1955. 
  data$c2.PostWarLiberal[data$ccode2==713 & data$year>1946 & data$year<1992]<- 1 # Republic of China ("Taiwan") concluded its consitution in 1947. 
  
  
  # Post World War II Communist Order from 1945 - 1991 (i.e., "Second World" aligned against NATO) See: https://history.state.gov/milestones/1953-1960/warsaw-treaty 
  # for Warsaw Pact countries.  Note that Soviet states do not have individual year dyad information in the dataset. 
  
  # Below are Warsaw Pact States, other communist states, and states aligned with the Soviet sphere but not formally allied. 
  
  data$c1.PostWarCommunist<-0
  data$c1.PostWarCommunist[data$ccode1==339 & data$year>1954 & data$year<1992]<- 1 # Albania
  data$c1.PostWarCommunist[data$ccode1==100 & data$year>1954 & data$year<1992]<- 1 # Bulgaria
  data$c1.PostWarCommunist[data$ccode1==315 & data$year>1954 & data$year<1992]<- 1 # Czechoslovakia
  data$c1.PostWarCommunist[data$ccode1==265 & data$year>1954 & data$year<1991]<- 1 # German Democratic Republic through 1990
  data$c1.PostWarCommunist[data$ccode1==310 & data$year>1954 & data$year<1992]<- 1 # Hungary
  data$c1.PostWarCommunist[data$ccode1==290 & data$year>1954 & data$year<1992]<- 1 # Poland
  data$c1.PostWarCommunist[data$ccode1==360 & data$year>1954 & data$year<1992]<- 1 # Romania
  data$c1.PostWarCommunist[data$ccode1==365 & data$year>1954 & data$year<1992]<- 1 # Russia
  # data$c1.PostWarCommunist[data$ccode1==375 & data$year>1944 & data$year<1992]<- 1 # Finland was neutral but leaned toward the Soviets in the latter part of the Cold War. 
  data$c1.PostWarCommunist[data$ccode1==710 & data$year>1948 & data$year<1992]<- 1 # People's Republic of China founded in 1949
  data$c1.PostWarCommunist[data$ccode1==731 & data$year>1947 & data$year<1992]<- 1 # Democratic People's Republic of Korea founded in 1948. Previously Soviet occupied.
  data$c1.PostWarCommunist[data$ccode1==812 & data$year>1948 & data$year<1992]<- 1 # Lao Peoples Democratic Republic gained independence in 1949.
  data$c1.PostWarCommunist[data$ccode1==712 & data$year>1944 & data$year<1992]<- 1 # Mongolia
  data$c1.PostWarCommunist[data$ccode1==816 & data$year>1944 & data$year<1992]<- 1 # Socialist Republic of Vietnam gained independence in 1945.
  data$c1.PostWarCommunist[data$ccode1==371 & data$year>1944 & data$year<1992]<- 1 # Armenia was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==373 & data$year>1944 & data$year<1992]<- 1 # Azerbaijan was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==370 & data$year>1944 & data$year<1992]<- 1 # Belarus was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==366 & data$year>1944 & data$year<1992]<- 1 # Estonia was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==372 & data$year>1944 & data$year<1992]<- 1 # Georgia was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==705 & data$year>1944 & data$year<1992]<- 1 # Kazakhstan was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==703 & data$year>1944 & data$year<1992]<- 1 # Kyrgyzstan was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==367 & data$year>1944 & data$year<1992]<- 1 # Latvia was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==368 & data$year>1944 & data$year<1992]<- 1 # Lithuania was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==359 & data$year>1944 & data$year<1992]<- 1 # Moldova was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==702 & data$year>1944 & data$year<1992]<- 1 # Tajikistan was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==701 & data$year>1944 & data$year<1992]<- 1 # Turkmenistan was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==369 & data$year>1944 & data$year<1992]<- 1 # Ukraine was part of the Soviet Union.
  data$c1.PostWarCommunist[data$ccode1==704 & data$year>1944 & data$year<1992]<- 1 # Uzbekistan was part of the Soviet Union.
  
  data$c2.PostWarCommunist<-0
  data$c2.PostWarCommunist[data$ccode2==339 & data$year>1954 & data$year<1992]<- 1 # Albania
  data$c2.PostWarCommunist[data$ccode2==100 & data$year>1954 & data$year<1992]<- 1 # Bulgaria
  data$c2.PostWarCommunist[data$ccode2==315 & data$year>1954 & data$year<1992]<- 1 # Czechoslovakia
  data$c2.PostWarCommunist[data$ccode2==265 & data$year>1954 & data$year<1991]<- 1 # German Democratic Republic through 1990
  data$c2.PostWarCommunist[data$ccode2==310 & data$year>1954 & data$year<1992]<- 1 # Hungary
  data$c2.PostWarCommunist[data$ccode2==290 & data$year>1954 & data$year<1992]<- 1 # Poland
  data$c2.PostWarCommunist[data$ccode2==360 & data$year>1954 & data$year<1992]<- 1 # Romania
  data$c2.PostWarCommunist[data$ccode2==365 & data$year>1954 & data$year<1992]<- 1 # Russia
  # data$c2.PostWarCommunist[data$ccode2==375 & data$year>1944 & data$year<1992]<- 1 # Finland was neutral but leaned toward the Soviets in the latter part of the Cold War. 
  data$c2.PostWarCommunist[data$ccode2==710 & data$year>1948 & data$year<1992]<- 1 # People's Republic of China founded in 1949
  data$c2.PostWarCommunist[data$ccode2==731 & data$year>1947 & data$year<1992]<- 1 # Democratic People's Republic of Korea founded in 1948. Previously Soviet occupied.
  data$c2.PostWarCommunist[data$ccode2==812 & data$year>1948 & data$year<1992]<- 1 # Lao Peoples Democratic Republic gained independence in 1949.
  data$c2.PostWarCommunist[data$ccode2==712 & data$year>1944 & data$year<1992]<- 1 # Mongolia
  data$c2.PostWarCommunist[data$ccode2==816 & data$year>1944 & data$year<1992]<- 1 # Socialist Republic of Vietnam gained independence in 1945.
  data$c2.PostWarCommunist[data$ccode2==371 & data$year>1944 & data$year<1992]<- 1 # Armenia was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==373 & data$year>1944 & data$year<1992]<- 1 # Azerbaijan was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==370 & data$year>1944 & data$year<1992]<- 1 # Belarus was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==366 & data$year>1944 & data$year<1992]<- 1 # Estonia was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==372 & data$year>1944 & data$year<1992]<- 1 # Georgia was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==705 & data$year>1944 & data$year<1992]<- 1 # Kazakhstan was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==703 & data$year>1944 & data$year<1992]<- 1 # Kyrgyzstan was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==367 & data$year>1944 & data$year<1992]<- 1 # Latvia was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==368 & data$year>1944 & data$year<1992]<- 1 # Lithuania was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==359 & data$year>1944 & data$year<1992]<- 1 # Moldova was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==702 & data$year>1944 & data$year<1992]<- 1 # Tajikistan was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==701 & data$year>1944 & data$year<1992]<- 1 # Turkmenistan was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==369 & data$year>1944 & data$year<1992]<- 1 # Ukraine was part of the Soviet Union.
  data$c2.PostWarCommunist[data$ccode2==704 & data$year>1944 & data$year<1992]<- 1 # Uzbekistan was part of the Soviet Union.
  
  # Warsaw Pact countries
  data$c1.WarsawPact<-0
  data$c1.WarsawPact[data$ccode1==339 & data$year>1954 & data$year<1969]<- 1 # Albania through 1968
  data$c1.WarsawPact[data$ccode1==100 & data$year>1954 & data$year<1992]<- 1 # Bulgaria
  data$c1.WarsawPact[data$ccode1==315 & data$year>1954 & data$year<1992]<- 1 # Czechoslovakia
  data$c1.WarsawPact[data$ccode1==265 & data$year>1954 & data$year<1991]<- 1 # German Democratic Republic through 1990
  data$c1.WarsawPact[data$ccode1==310 & data$year>1954 & data$year<1992]<- 1 # Hungary
  data$c1.WarsawPact[data$ccode1==290 & data$year>1954 & data$year<1992]<- 1 # Poland
  data$c1.WarsawPact[data$ccode1==360 & data$year>1954 & data$year<1992]<- 1 # Romania
  data$c1.WarsawPact[data$ccode1==365 & data$year>1954 & data$year<1992]<- 1 # Russia
  
  data$c2.WarsawPact<-0
  data$c2.WarsawPact[data$ccode2==339 & data$year>1954 & data$year<1969]<- 1 # Albania through 1968
  data$c2.WarsawPact[data$ccode2==100 & data$year>1954 & data$year<1992]<- 1 # Bulgaria
  data$c2.WarsawPact[data$ccode2==315 & data$year>1954 & data$year<1992]<- 1 # Czechoslovakia
  data$c2.WarsawPact[data$ccode2==265 & data$year>1954 & data$year<1991]<- 1 # German Democratic Republic through 1990
  data$c2.WarsawPact[data$ccode2==310 & data$year>1954 & data$year<1992]<- 1 # Hungary
  data$c2.WarsawPact[data$ccode2==290 & data$year>1954 & data$year<1992]<- 1 # Poland
  data$c2.WarsawPact[data$ccode2==360 & data$year>1954 & data$year<1992]<- 1 # Romania
  data$c2.WarsawPact[data$ccode2==365 & data$year>1954 & data$year<1992]<- 1 # Russia
  
  
  # Post World War II states that were in neither camp.  This includes both un-aligned middle powers and those states previously referred to as the "third world." 
  
  data$c1.PostWarOther<-0
  data$c1.PostWarOther[data$c1.PostWarCommunist==0 & data$c1.PostWarLiberal==0 & data$year>1944 & data$year<1992]<-1
  
  data$c2.PostWarOther<-0
  data$c2.PostWarOther[data$c2.PostWarCommunist==0 & data$c2.PostWarLiberal==0 & data$year>1944 & data$year<1992]<-1
  
  # Post-Cold War Western order (assumed to be end-of-CW coalition)
  
  data$c1.PostCWLiberal<-0
  data$c1.PostCWLiberal[data$ccode1==211 & data$year>1991]<- 1 # Belgium
  data$c1.PostCWLiberal[data$ccode1==20 & data$year>1991]<- 1 # Canada
  data$c1.PostCWLiberal[data$ccode1==390 & data$year>1991]<- 1 # Denmark
  data$c1.PostCWLiberal[data$ccode1==220 & data$year>1991]<- 1 # France
  data$c1.PostCWLiberal[data$ccode1==395 & data$year>1991]<- 1 # Iceland
  data$c1.PostCWLiberal[data$ccode1==325 & data$year>1991]<- 1 # Italy
  data$c1.PostCWLiberal[data$ccode1==212 & data$year>1991]<- 1 # Luxembourg
  data$c1.PostCWLiberal[data$ccode1==210 & data$year>1991]<- 1 # Netherlands
  data$c1.PostCWLiberal[data$ccode1==385 & data$year>1991]<- 1 # Norway
  data$c1.PostCWLiberal[data$ccode1==235 & data$year>1991]<- 1 # Portugal
  data$c1.PostCWLiberal[data$ccode1==200 & data$year>1991]<- 1 # United Kingdom
  data$c1.PostCWLiberal[data$ccode1==2 & data$year>1991]<- 1 # United States
  data$c1.PostCWLiberal[data$ccode1==350 & data$year>1991]<- 1 # Greece joined NATO in 1952
  data$c1.PostCWLiberal[data$ccode1==640 & data$year>1991]<- 1 # Turkey joined NATO in 1952
  data$c1.PostCWLiberal[data$ccode1==260 & data$year>1991]<- 1 # German Federal Republic joined NATO in 1955 and was a member through October 1990
  data$c1.PostCWLiberal[data$ccode1==255 & data$year>1991]<- 1 # Unified Germany joined NATO in October 1990 ## note overlap with W. Germany in 1990.
  data$c1.PostCWLiberal[data$ccode1==230 & data$year>1991]<- 1 # Spain joined NATO in 1982
  data$c1.PostCWLiberal[data$ccode1==666 & data$year>1991]<- 1 # Israel founded in 1948
  data$c1.PostCWLiberal[data$ccode1==740 & data$year>1991]<- 1 # Japan: US occupation ended in 1952. See: http://afe.easia.columbia.edu/special/japan_1900_occupation.htm
  data$c1.PostCWLiberal[data$ccode1==732 & data$year>1991]<- 1 # Republic of Korea. US occupation ended in 1948. Note CCODE 732 is the ROK specific code.
  data$c1.PostCWLiberal[data$ccode1==900 & data$year>1991]<- 1 # Australia. Note that the ANZUS treaty started in 1951 but Australia was aligned with West prior. 
  data$c1.PostCWLiberal[data$ccode1==920 & data$year>1991]<- 1 # New Zealand. Note that the ANZUS treaty started in 1951 but NZ was aligned with West prior. 
  data$c1.PostCWLiberal[data$ccode1==305 & data$year>1991]<- 1 # Austria was western, liberal democratic but neutral. Note CCODE 305 is the Austria-specific code. There is a separate Austria-Hungary code (300)
  data$c1.PostCWLiberal[data$ccode1==205 & data$year>1991]<- 1 # Ireland was western, liberal democratic but neutral.
  data$c1.PostCWLiberal[data$ccode1==380 & data$year>1991]<- 1 # Sweden was western, liberal democratic but neutral.
  data$c1.PostCWLiberal[data$ccode1==225 & data$year>1991]<- 1 # Switzerland was western, liberal democratic but neutral.
  data$c1.PostCWLiberal[data$ccode1==713 & data$year>1991]<- 1 # Republic of China ("Taiwan") concluded its consitution in 1947. 
  
  
  
  data$c2.PostCWLiberal<-0
  data$c2.PostCWLiberal[data$ccode2==211 & data$year>1991]<- 1 # Belgium
  data$c2.PostCWLiberal[data$ccode2==20 & data$year>1991]<- 1 # Canada
  data$c2.PostCWLiberal[data$ccode2==390 & data$year>1991]<- 1 # Denmark
  data$c2.PostCWLiberal[data$ccode2==220 & data$year>1991]<- 1 # France
  data$c2.PostCWLiberal[data$ccode2==395 & data$year>1991]<- 1 # Iceland
  data$c2.PostCWLiberal[data$ccode2==325 & data$year>1991]<- 1 # Italy
  data$c2.PostCWLiberal[data$ccode2==212 & data$year>1991]<- 1 # Luxembourg
  data$c2.PostCWLiberal[data$ccode2==210 & data$year>1991]<- 1 # Netherlands
  data$c2.PostCWLiberal[data$ccode2==385 & data$year>1991]<- 1 # Norway
  data$c2.PostCWLiberal[data$ccode2==235 & data$year>1991]<- 1 # Portugal
  data$c2.PostCWLiberal[data$ccode2==200 & data$year>1991]<- 1 # United Kingdom
  data$c2.PostCWLiberal[data$ccode2==2 & data$year>1991]<- 1 # United States
  data$c2.PostCWLiberal[data$ccode2==350 & data$year>1991]<- 1 # Greece joined NATO in 1952
  data$c2.PostCWLiberal[data$ccode2==640 & data$year>1991]<- 1 # Turkey joined NATO in 1952
  data$c2.PostCWLiberal[data$ccode2==260 & data$year>1991]<- 1 # German Federal Republic joined NATO in 1955 and was a member through October 1990
  data$c2.PostCWLiberal[data$ccode2==255 & data$year>1991]<- 1 # Unified Germany joined NATO in October 1990 ## note overlap with W. Germany in 1990.
  data$c2.PostCWLiberal[data$ccode2==230 & data$year>1991]<- 1 # Spain joined NATO in 1982
  data$c2.PostCWLiberal[data$ccode2==666 & data$year>1991]<- 1 # Israel founded in 1948
  data$c2.PostCWLiberal[data$ccode2==740 & data$year>1991]<- 1 # Japan: US occupation ended in 1952. See: http://afe.easia.columbia.edu/special/japan_1900_occupation.htm
  data$c2.PostCWLiberal[data$ccode2==732 & data$year>1991]<- 1 # Republic of Korea. US occupation ended in 1948. Note CCODE 732 is the ROK specific code.
  data$c2.PostCWLiberal[data$ccode2==900 & data$year>1991]<- 1 # Australia. Note that the ANZUS treaty started in 1951 but Australia was aligned with West prior. 
  data$c2.PostCWLiberal[data$ccode2==920 & data$year>1991]<- 1 # New Zealand. Note that the ANZUS treaty started in 1951 but NZ was aligned with West prior. 
  data$c2.PostCWLiberal[data$ccode2==305 & data$year>1991]<- 1 # Austria was western, liberal democratic but neutral. Note CCODE 305 is the Austria-specific code. There is a separate Austria-Hungary code (300)
  data$c2.PostCWLiberal[data$ccode2==205 & data$year>1991]<- 1 # Ireland was western, liberal democratic but neutral.
  data$c2.PostCWLiberal[data$ccode2==380 & data$year>1991]<- 1 # Sweden was western, liberal democratic but neutral.
  data$c2.PostCWLiberal[data$ccode2==225 & data$year>1991]<- 1 # Switzerland was western, liberal democratic but neutral.
  data$c2.PostCWLiberal[data$ccode2==713 & data$year>1991]<- 1 # Republic of China ("Taiwan") concluded its consitution in 1947. 
  
  # NATO expansion countries; coding based on status as of 1/1/year
  data$c1.PostCWLiberal[data$ccode1==316 & data$year>1999]<- 1 # Czech Republic
  data$c1.PostCWLiberal[data$ccode1==290 & data$year>1999]<- 1 # Poland
  data$c1.PostCWLiberal[data$ccode1==310 & data$year>1999]<- 1 # Hungary
  data$c1.PostCWLiberal[data$ccode1==355 & data$year>2004]<- 1 # Bulgaria
  data$c1.PostCWLiberal[data$ccode1==366 & data$year>2004]<- 1 # Estonia
  data$c1.PostCWLiberal[data$ccode1==367 & data$year>2004]<- 1 # Latvia
  data$c1.PostCWLiberal[data$ccode1==368 & data$year>2004]<- 1 # Lithuania
  data$c1.PostCWLiberal[data$ccode1==360 & data$year>2004]<- 1 # Romania
  data$c1.PostCWLiberal[data$ccode1==317 & data$year>2004]<- 1 # Slovakia
  data$c1.PostCWLiberal[data$ccode1==349 & data$year>2004]<- 1 # Slovenia
  data$c1.PostCWLiberal[data$ccode1==339 & data$year>2009]<- 1 # Albania
  data$c1.PostCWLiberal[data$ccode1==344 & data$year>2009]<- 1 # Croatia
  data$c2.PostCWLiberal[data$ccode2==316 & data$year>1999]<- 1 # Czech Republic
  data$c2.PostCWLiberal[data$ccode2==290 & data$year>1999]<- 1 # Poland
  data$c2.PostCWLiberal[data$ccode2==310 & data$year>1999]<- 1 # Hungary
  data$c2.PostCWLiberal[data$ccode2==355 & data$year>2004]<- 1 # Bulgaria
  data$c2.PostCWLiberal[data$ccode2==366 & data$year>2004]<- 1 # Estonia
  data$c2.PostCWLiberal[data$ccode2==367 & data$year>2004]<- 1 # Latvia
  data$c2.PostCWLiberal[data$ccode2==368 & data$year>2004]<- 1 # Lithuania
  data$c2.PostCWLiberal[data$ccode2==360 & data$year>2004]<- 1 # Romania
  data$c2.PostCWLiberal[data$ccode2==317 & data$year>2004]<- 1 # Slovakia
  data$c2.PostCWLiberal[data$ccode2==349 & data$year>2004]<- 1 # Slovenia
  data$c2.PostCWLiberal[data$ccode2==339 & data$year>2009]<- 1 # Albania
  data$c2.PostCWLiberal[data$ccode2==344 & data$year>2009]<- 1 # Croatia
  # data$c2.PostCWLiberal[data$ccode2==341 & data$year>2017]<- 1 # Montenegro
  
  # EU expansion (not including those predated by NATO expansion); 1/1/year
  data$c1.PostCWLiberal[data$ccode1==375 & data$year>1994]<- 1 # Finland
  data$c1.PostCWLiberal[data$ccode1==352 & data$year>2004]<- 1 # Cyprus
  data$c1.PostCWLiberal[data$ccode1==338 & data$year>2004]<- 1 # Malta
  data$c2.PostCWLiberal[data$ccode2==375 & data$year>1994]<- 1 # Finland
  data$c2.PostCWLiberal[data$ccode2==352 & data$year>2004]<- 1 # Cyprus
  data$c2.PostCWLiberal[data$ccode2==338 & data$year>2004]<- 1 # Malta
  
  
  data$order <- 0  
  data$order[(data$c1.concert==1 & data$c2.concert==1) | (data$c1.bismarck==1 & data$c2.bismarck==1) | (data$c1.League==1 & data$c2.League==1) | (data$c1.PostWarLiberal==1 & data$c2.PostWarLiberal==1) | (data$c1.PostWarCommunist==1 & data$c2.PostWarCommunist==1) | (data$c1.PostCWLiberal==1 & data$c2.PostCWLiberal==1)] <- 1
  
  data$outsideorder <- 0  
  data$outsideorder[(data$c1.concert==1 & data$c2.concert==0) | (data$c1.bismarck==1 & data$c2.bismarck==0) | (data$c1.League==1 & data$c2.League==0 & !(data$ccode2 %in% c(255,365,740))) | (data$c1.concert==0 & data$c2.concert==1) | (data$c1.bismarck==0 & data$c2.bismarck==1) | (data$c1.League==0 & !(data$ccode1 %in% c(255,365,740)) & data$c2.League==1) | (data$c1.PostCWLiberal==1 & data$c2.PostCWLiberal==0) | (data$c1.PostCWLiberal==0 & data$c2.PostCWLiberal==1)] <- 1
  
  data$betweenorders <- 0
  data$betweenorders[(data$c1.League==1 & data$ccode2 %in% c(255,365,740)) | (data$ccode1 %in% c(255,365,740) & data$c2.League==1) | (data$c1.PostWarLiberal==1 & data$c2.PostWarLiberal==0) | (data$c1.PostWarLiberal==0 & data$c2.PostWarLiberal==1) | (data$c1.PostWarCommunist==1 & data$c2.PostWarCommunist==0) | (data$c1.PostWarCommunist==0 & data$c2.PostWarCommunist==1)] <- 1
  
  data$beyondorders <- 0
  data$beyondorders[
    data$order == 0 &
      data$betweenorders == 0 &
      data$outsideorder == 0
  ] <- 1
  
  data$order_status <- "Same Order"
  data$order_status[
    data$outsideorder == 1 
  ] <- "In vs. Out"
  data$order_status[
    data$beyondorders == 1
  ] <- "Outside Order"
  data$order_status[
    data$betweenorders == 1
  ] <- "Competing Orders"
  
  ## return the data
  data
}

