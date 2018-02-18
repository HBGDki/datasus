census_dict <- list(
  # Rate of illiteracy
  ALFBR = list(
    MUNCOD = list(
      type     = "character",
      name_en  = "muni_code",
      label_en = "Municipality code"
    ),
    ANO        = list(
      type     = "numeric",
      name_en  = "year",
      label_en = "Year of the data"
    ),
    CORRACA    = list(
      type     = "factor",
      name_en  = "race",
      label_en = "color / race",
      map_en   = c("1" = "White",
                   "2" = "Black",
                   "3" = "Asian",
                   "4" = "Multiracial",
                   "5" = "Indigenous",
                   "0" = "No declaration")
    ),
    SEXO       = list(
      type     = "factor",
      name_en  = "sex",
      label_en = "sex",
      map_en   = c("M" = "Male",
                   "F" = "Female")
    ),
    SITUACAO   = list(
      type     = "factor",
      name_en  = "situation",
      label_en = "situation of domicile",
      map_en   = c("U" = "Urban",
                   "R" = "Rural")
    ),
    IDADE      = list(
      type     = "factor",
      name_en  = "age_group",
      label_en = "Age group",
      map_en   = c("1524" = "15 to 24 years",
                   "2559" = "25 to 59 years",
                   "6099" = "60 years and over")
    ),
    POPALFAB   = list(
      type     = "numeric",
      name_en  = "pop_lit",
      label_en = "literate resident population"
    ),
    POPNALFAB  = list(
      type     = "numeric",
      name_en  = "pop_illit",
      label_en = "illitirate resident population"
    ),
    POPTOT     = list(
      type     = "numeric",
      name_en  = "pop_tot",
      label_en = "total resident population"
    )
  ),
  # Population education of 15 years and over
  ESCABR = list(
    MUNCOD = list(
      type     = "character",
      name_en  = "muni_code",
      label_en = "Municipality code"
    ),
    ANO        = list(
      type     = "numeric",
      name_en  = "year",
      label_en = "Year of the data"
    ),
    CORRACA    = list(
      type     = "factor",
      name_en  = "race",
      label_en = "color / race",
      map_en   = c("1" = "White",
                   "2" = "Black",
                   "3" = "Asian",
                   "4" = "Multiracial",
                   "5" = "Indigenous",
                   "0" = "No declaration")
    ),
    SEXO       = list(
      type     = "factor",
      name_en  = "sex",
      label_en = "sex",
      map_en   = c("M" = "Male",
                   "F" = "Female")
    ),
    SITUACAO   = list(
      type     = "factor",
      name_en  = "situation",
      label_en = "situation of domicile",
      map_en   = c("U" = "Urban",
                   "R" = "Rural")
    ),
    ESCOLARID  = list(
      type     = "factor",
      name_en  = "schooling",
      label_en = "schooling",
      map_en   = c("0000" = "less than 1 year of study", # (1991 and 2000 data) *
                   "0003" = "1 to 3 years of study", # (2010 data???) *
                   "0103" = "1 to 3 years of study", # (1991 and 2000 data) *
                   "0407" = "4 to 7 years of study", # (1991 and 2000 data) *
                   "0899" = "8 years or more of study", # (1991 and 2000 data) *
                   # "SI1I" = "No education / 1st incomplete fundamental cycle", # (2010 data)
                   # "1C2I" = "1st complete cycle complete / 2nd cycle incomplete", # (2010 data)
                   # "2C99" = "2nd complete or complete fundamental cycle", # (2010 data)
                   "ALFA" = "Adult literacy", # (data from 2000) *
                   "IGNO" = "Not determined") # *
    ),
    POPTOT     = list(
      type     = "numeric",
      name_en  = "pop_tot",
      label_en = "total resident population of 15 years or more"
    )
  ),
  # Education of the population aged 18 to 24
  ESCBBR = list(
    MUNCOD = list(
      type     = "character",
      name_en  = "muni_code",
      label_en = "Municipality code"
    ),
    ANO        = list(
      type     = "numeric",
      name_en  = "year",
      label_en = "Year of the data"
    ),
    CORRACA    = list(
      type     = "factor",
      name_en  = "race",
      label_en = "color / race",
      map_en   = c("1" = "White",
                   "2" = "Black",
                   "3" = "Asian",
                   "4" = "Multiracial",
                   "5" = "Indigenous",
                   "0" = "No declaration")
    ),
    SEXO       = list(
      type     = "factor",
      name_en  = "sex",
      label_en = "sex",
      map_en   = c("M" = "Male",
                   "F" = "Female")
    ),
    SITUACAO   = list(
      type     = "factor",
      name_en  = "situation",
      label_en = "situation of domicile",
      map_en   = c("U" = "Urban",
                   "R" = "Rural")
    ),
    ESCOLARID  = list(
      type     = "factor",
      name_en  = "schooling",
      label_en = "schooling",
      map_en   = c("0000" = "less than 1 year of study", # (1991 and 2000 data) *
                   "0003" = "1 to 3 years of study", # (2010 data???) *
                   "0103" = "1 to 3 years of study", # (1991 and 2000 data) *
                   "0407" = "4 to 7 years of study", # (1991 and 2000 data) *
                   "0899" = "8 years or more of study", # (1991 and 2000 data) *
                   # "SI1I" = "No education / 1st incomplete fundamental cycle", # (2010 data)
                   # "1C2I" = "1st complete cycle complete / 2nd cycle incomplete", # (2010 data)
                   # "2C99" = "2nd complete or complete fundamental cycle", # (2010 data)
                   "ALFA" = "Adult literacy", # (data from 2000) *
                   "IGNO" = "Not determined") # *
    ),
    POPTOT     = list(
      type     = "numeric",
      name_en  = "pop_tot",
      label_en = "total resident population of 15 years or more"
    )
  ),
  # Proportion of elderly living in households in the condition of another relative
  IDOSOBR = list(
    MUNCOD = list(
      type     = "character",
      name_en  = "muni_code",
      label_en = "Municipality code"
    ),
    ANO        = list(
      type     = "numeric",
      name_en  = "year",
      label_en = "Year of the data"
    ),
    CORRACA    = list(
      type     = "factor",
      name_en  = "race",
      label_en = "color / race",
      map_en   = c("1" = "White",
                   "2" = "Black",
                   "3" = "Asian",
                   "4" = "Multiracial",
                   "5" = "Indigenous",
                   "0" = "No declaration")
    ),
    SEXO       = list(
      type     = "factor",
      name_en  = "sex",
      label_en = "sex",
      map_en   = c("M" = "Male",
                   "F" = "Female")
    ),
    IDADE      = list(
      type     = "factor",
      name_en  = "age_group",
      label_en = "Age group",
      map_en   = c("6069" = "60 to 69 years",
                   "7079" = "70 to 79 years",
                   "8099" = "80 years and over")
    ),
    POPDEPEND  = list(
      type     = "numeric",
      name_en  = "pop_depend",
      label_en = "population of elderly people living in households in the condition of another relative"
    ),
    POPTOT     = list(
      type     = "numeric",
      name_en  = "pop_tot",
      label_en = "total resident population"
    )
  ),
  # Average household income per capita
  RENDABR = list(
    MUNCOD = list(
      type     = "character",
      name_en  = "muni_code",
      label_en = "Municipality code"
    ),
    ANO        = list(
      type     = "numeric",
      name_en  = "year",
      label_en = "Year of the data"
    ),
    CORRACA    = list(
      type     = "factor",
      name_en  = "race",
      label_en = "color / race",
      map_en   = c("1" = "White",
                   "2" = "Black",
                   "3" = "Asian",
                   "4" = "Multiracial",
                   "5" = "Indigenous",
                   "0" = "No declaration")
    ),
    SITUACAO   = list(
      type     = "factor",
      name_en  = "situation",
      label_en = "situation of domicile",
      map_en   = c("I" = "???") # get rid of this variable
    ),
    NUMRENDA   = list(
      type     = "numeric",
      name_en  = "house_inc",
      label_en = "sum of the average household income (numerator)"
    ),
    DENRENDA   = list(
      type     = "numeric",
      name_en  = "pop",
      label_en = "population considered (denominator)"
    ),
    DENCRIREND = list(
      type     = "numeric",
      name_en  = "n_child",
      label_en = "number of children considered"
    ),
    NUMPOBRES  = list(
      type     = "numeric",
      name_en  = "pop_2mw",
      label_en = "population with average household income per capita less than 1/2 minimum wage"
    ),
    NUMPOBRESX = list(
      type     = "numeric",
      name_en  = "pop_4mw",
      label_en = "population with average household income per capita less than 1/4 minimum wage"
    ),
    NUMCRIPOB  = list(
      type     = "numeric",
      name_en  = "n_child_2mw",
      label_en = "children in a situation of average household income per capita less than 1/2 minimum wage"
    ),
    NUMCRIPOBX = list(
      type     = "numeric",
      name_en  = "n_child_4mw",
      label_en = "children in a situation of average household income per capita less than 1/4 minimum wage"
    ),
    NUMDESOCUP = list(
      type     = "numeric",
      name_en  = "pop_16unemp",
      label_en = "resident economically active population aged 16 and over who are unemployed"
    ),
    DENDESOCUP = list(
      type     = "numeric",
      name_en  = "pop_16",
      label_en = "resident economically active population aged 16 and over"
    ),
    NUMTRABINF = list(
      type     = "numeric",
      name_en  = "pop_10work",
      label_en = "resident population with 10 to 15 years of age who is working or looking for work"
    ),
    DENTRABINF = list(
      type     = "numeric",
      name_en  = "pop_10",
      label_en = "total resident population with 10 to 15 years of age"
    )
  )
)
