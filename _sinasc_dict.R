## a data dictionary for the SINASC data
##---------------------------------------------------------

sinasc_dict <- list(
  NUMERODN   = list(
    type     = "character",
    name_en  = "dn_number",
    label_en = "DN number sequential by UF and year",
    label    = "Número da DN, seqüencial por UF informante e por ano"
  ),
  LOCNASC    = list(
    type     = "factor",
    name_en  = "birth_place",
    label_en = "Place of birth",
    map_en   = c("1" = "Hospital",
                 "2" = "Other Health Estab",
                 "3" = "Home",
                 "4" = "Other",
                 "9" = NA),
    label    = "Local de ocorrência do nascimento",
    map      = c("1" = "Hospital",
                 "2" = "Outro Estab Saúde",
                 "3" = "Domicílio",
                 "4" = "Outros",
                 "9" = NA)
  ),
  CODESTAB   = list(
    type     = "character",
    name_en  = "health_estbl_code",
    label_en = "Health establishment code",
    label    = "Código de estabelecimento de saúde"
  ),
  CODBAINASC = list(
    type     = "character",
    name_en  = "birth_nbhd_code",
    label_en = "Birth neighborhood code",
    label    = "Código Bairro nascimento"
  ),
  CODMUNNASC = list(
    type     = "character",
    name_en  = "birth_muni_code",
    label_en = "Birth municipal code",
    label    = "Código do município de ocorrência"
  ),
  IDADEMAE   = list(
    type     = "numeric",
    name_en  = "m_age_yrs",
    label_en = "Age of the mother in years",
    label    = "Idade da mãe em anos"
  ),
  ESTCIVMAE  = list(
    type     = "factor",
    name_en  = "marital_status",
    label_en = "Mother's marital status",
    map_en   = c("1" = "Single",
                 "2" = "Married",
                 "3" = "Widow",
                 "4" = "Separated / Divorced",
                 "5" = "Single", # Consensual Union (pre-2005 but we want consistent mappings)
                 "9" = NA),
    label    = "Estado civil",
    map      = c("1" = "Solteira",
                 "2" = "Casada",
                 "3" = "Viúva",
                 "4" = "Separado judicialmente/Divorciado",
                 "5" = "Solteira", # União consensual (versões anteriores)
                 "9" = NA)
  ),
  ESCMAE     = list(
    type     = "factor",
    name_en  = "m_educ",
    label_en = "Mother's education",
    map_en   = c("1" = "None",
                 "2" = "1 to 3 years",
                 "3" = "4 to 7 years",
                 "4" = "8 to 11 years",
                 "5" = "12 and more",
                 "9" = NA),
    label    = "Escolaridade, anos de estudo concluídos",
    map      = c("1" = "Nenhuma",
                 "2" = "1 a 3anos",
                 "3" = "4 a 7anos",
                 "4" = "8 a 11anos",
                 "5" = "12 e mais",
                 "9" = NA)
  ),
  CODOCUPMAE = list(
    type     = "character",
    name_en  = "occ_code",
    label_en = "Mother's occupation, according to the Brazilian Occupations (CBO-2002)",
    label    = "Ocupação, conforme a Classificação Brasileira de Ocupações (CBO-2002)"
  ),
  QTDFILVIVO = list(
    type     = "numeric",
    name_en  = "n_live_child",
    label_en = "Number of living children",
    label    = "Número de filhos vivos"
  ),
  QTDFILMORT = list(
    type     = "numeric",
    name_en  = "n_dead_child",
    label_en = "Number of deceased children",
    label    = "Número de filhos mortos"
  ),
  CODBAIRES  = list(
    type     = "character",
    name_en  = "res_nbhd_code",
    label_en = "Residence neighborhood code",
    label    = "Código bairro residência"
  ),
  CODMUNRES  = list(
    type     = "character",
    name_en  = "m_muni_code",
    label_en = "Residence municipal code of the mother",
    label    = "Município de residência da mãe"
  ),
  GESTACAO   = list(
    type     = "factor",
    name_en  = "gest_weeks",
    label_en = "Weeks of gestation",
    map_en   = c("1" = "Less than 22 weeks",
                 "2" = "22-27 weeks",
                 "3" = "28 to 31 weeks",
                 "4" = "32-36 weeks",
                 "5" = "37-41 weeks",
                 "6" = "42 weeks and more",
                 "9" = NA),
    label    = "Semanas de gestação",
    map      = c("1" = "Menos de 22 semanas",
                 "2" = "22 a 27 semanas",
                 "3" = "28 a 31 semanas",
                 "4" = "32 a 36 semanas",
                 "5" = "37 a 41 semanas",
                 "6" = "42 semanas e mais",
                 "9" = NA)
  ),
  GRAVIDEZ   = list(
    type     = "factor",
    name_en  = "preg_type",
    label_en = "Type of pregnancy",
    map_en   = c("1" = "Singleton",
                 "2" = "Twins",
                 "3" = "Triplets and more",
                 "9" = NA),
    label    = "Tipo de gravidez",
    map      = c("1" = "Única",
                 "2" = "Dupla",
                 "3" = "Tripla e mais",
                 "9" = NA)
  ),
  PARTO      = list(
    type     = "factor",
    name_en  = "deliv_type",
    label_en = "Type of delivery",
    map_en   = c("1" = "Vaginal",
                 "2" = "Cesarean",
                 "9" = NA),
    label    = "Tipo de parto",
    map      = c("1" = "Vaginal",
                 "2" = "Cesáreo",
                 "9" = NA)
  ),
  CONSULTAS  = list(
    type     = "factor",
    name_en  = "n_prenatal_visit",
    label_en = "Number of prenatal visits",
    map_en   = c("1" = "None",
                 "2" = "1 - 3",
                 "3" = "4 - 6",
                 "4" = "7+",
                 "9" = NA),
    label    = "Número de consultas de pré-natal",
    map      = c("1" = "Nenhuma",
                 "2" = "de 1 a 3",
                 "3" = "de 4 a 6",
                 "4" = "7 e mais",
                 "9" = NA)
  ),
  DTNASC     = list(
    type     = "date",
    name_en  = "birth_date",
    label_en = "Date of birth in ddmmyyyy format",
    label    = "Data do nascimento, no formato ddmmaaaa"
  ),
  HORANASC   = list(
    type     =  "character", # for now
    name_en  = "birth_time",
    label_en = "Time of birth",
    label    = "Hora do nascimento"
  ),
  SEXO = list(
    type     = "factor",
    name_en  = "sex",
    label_en = "Sex",
    map_en   = c("1" = "Male",
                 "2" = "Female",
                 "0" = NA),
    label    = "Sexo",
    map      = c("1" = "Masculino",
                 "2" = "Feminino",
                 "0" = NA)
  ),
  APGAR1 = list(
    type     = "factor",
    name_en  = "apgar1",
    label_en = "Apgar in the first minute (00 to 10)",
    label    = "Apgar no primeiro minuto (00 a 10)"
  ),
  APGAR5 = list(
    type     = "factor",
    name_en  = "apgar5",
    label_en = "Apgar in the fifth minute (00 to 10)",
    label    = "Apgar no quinto minuto (00 a 10)"
  ),
  RACACOR = list(
    type     = "factor",
    name_en  = "race",
    label_en = "Race / Color",
    map_en   = c("1" = "White",
                 "2" = "Black",
                 "3" = "Asian",
                 "4" = "Multiracial",
                 "5" = "Indigenous"),
    label    = "Raça/Cor",
    map      = c("1" = "Branca",
                 "2" = "Preta",
                 "3" = "Amarela",
                 "4" = "Parda",
                 "5" = "Indígena")
  ),
  PESO       = list(
    type     = "numeric",
    name_en  = "brthwt_g",
    label_en = "Birth weight, in grams",
    label    = "Peso ao nascer, em gramas"
  ),
  IDANOMAL   = list(
    type     = "factor",
    name_en  = "cong_anom",
    label_en = "Congenital anomaly",
    map_en   = c("1" = "Yes",
                 "2" = "No",
                 "9" = NA),
    label    = "Anomalia congênita",
    map      = c("1" = "Sim",
                 "2" = "Não",
                 "9" = NA)
  ),
  CODANOMAL  = list(
    type     = "character",
    name_en  = "cong_icd10",
    label_en = "Code of congenital malformation or anomaly chromosome, according to ICD-10",
    label    = "Código de malformação congênita ou anomalia cromossômica, de acordo com a CID-10"
  ),
  DTCADASTRO = list(
    type     = "date",
    name_en  = "sys_reg_date",
    label_en = "Date of registration in the system",
    label    = "Data de cadastramento no sistema"
  ),
  DTRECEBIM  = list(
    type     = "date",
    name_en  = "rec_reg_date",
    label_en = "Receipt date at central level, last date registry update",
    label    = "Data de recebimento no nível central, data da última atualização do registro"
  ),
  CODINST    = list(
    type     = "character",
    name_en  = "reg_gen_code",
    label_en = "Registration generation installation code",
    label    = "Código da Instalação da geração dos Registros"
  ),
  UFINFORM   = list(
    type     = "character",
    name_en  = "rep_uf_code",
    label_en = "UF code that reported the record",
    label    = "Código da UF que informou o registro"
  )
)


# CODMUNCART
# RACACOR_RN
# DTDECLARAC
# PARIDADE
# RACACORN
# CODUFNATU
# ESCMAEAGR1
# CODCART
# DTREGCART
# ESCMAE2010
# NUMREGCART
# CONSPRENAT
# DTRECORIG
# TPROBSON
# CODMUNNATU
# CODPAISRES
# DIFDATA
# DTNASCMAE
# NATURALMAE
# NUMERODV
# NUMEROLOTE
# ORIGEM
# PREFIXODN
# QTDGESTANT
# QTDPARTCES
# QTDPARTNOR
# RACACORMAE
# SERIESCMAE
# STDNEPIDEM
# STDNNOVA
# VERSAOSIST


# 2005 differs at ESTCIVMAE
# 1: Solteira
# 2: Casada
# 3: Viuva
# 4: Separado judicialmente/Divorciado
# 5: União consensual (versões anteriores)
# 9: Ignorado
# 1: Single
# 2: Married
# 3: Widow
# 4: Separated judicially / Divorced
# 5: Consensual Union (previous versions)
# 9: Ignored

# http://svs.aids.gov.br/download/Dicionario_de_Dados_SINASC_tabela_DN.pdf


# DTULTMENST character 8 Date of last menstruation (DUM): dd mm yyyy
# SEMAGESTAC Weeks Gest Detail
# SEMADUM.CN V character 2 Number of weeks of gestation. Thread Tools Method Estimate
# TPMETODO.C NV character 2 Method used. Values: 1- Physical examination; 2- Another method; 9- Ignored.
# CODCRM character 10 Physician's CRM code.
# MESPRENAT 1st month Query
# MEGEST.CNV character 2 Month of gestation in which the prenatal period began
# TPAPRESENT character 2 Type of presentation of the RN. Values: 1- Cephalic; 2- Pelvic or breeding; 3- Transverse; 9- Ignored.
# STTRABPART character 1 Induced labor? Values: 1- Yes; 2- No; 3- No applies; 9- Ignored.
# STESPART Cesarea Ocorr Plymouth
# STPARTO.CNV character 1 Caesarean section occurred before labor began? Values: 1- Yes; 2- No; 3- Does not apply; 9- Ignored.

# TPNASCASSI Nas Assisted
# TPASSIST.CNV character 2 Nascimento was watched by? Values: 1- Medical; 2-Nurse / midwife; 3- Midwife; 4- Others; 9- Ignored.
# NOMERNASC character 70 Name of newborn
# NOMERESPO character 70 Name of person in charge
# TPFUNCRESP character 1 Type of function of the person in charge. Values: 1- Physician; 2- Nurses; 3- Midwife; 4- Employee of the registry; 5- Others. Dtrito
# CRESP character 70 Description of the function of the person responsible for completing the function for "5- Others".
# CODPROF character 15 Code of the profession, if the function is "5- Others".
# PROFISSION character 70 Name of the Professional if the function is "5- Others".
# COREN character 60 Name of nurse practitioner
# CODCOREN character 8 COREN Code
# TPDOCRESP character 1 Type of document of the person in charge. Values: 1-CNES; 2-CRM; 3- COREN; 4-RG; 5-CPF.
# NUDOCRESP character 20 Document number of the person in charge.
# CONSPRENAT Inquiries Detail
# SEMADUM.CN V character 2 Number of prenatal consultations
# ORGEMISER character 20 Description of the body issuing the document of the person responsible for fill.
# DTDECLARAC character 8 Date of declaration: dd mm yyyy
# ESCMAEAGR1 Escape Added ESCAGR1.CNV character 2 Education 2010 aggregate. Values: 00 - No Schooling; 01 - Fundamental I Incomplete; 02 - Fundamental I Complete; 03 - Fundamental II Incomplete; 04 - Fundamental II Complete; 05 - Secondary School Incomplete; 06 - High School Complete; 07 - Incomplete Superior; 08 - Superior Complete; 09 - Ignored; 10 - Fundamental I Incomplete or Nonspecific; 11 - Fundamental II Incomplete or Nonspecific; 12 - Teaching Medium Incomplete or Nonspecific.
# ESCMAEAGR2 character 2 --------------------
# TPROBSON Monitor cesarean
# ROBSON.CNV character 2 Robson Group Code generated by the system
# STDNEPIDEM DN epidemiological
# DNNOVA.CNV character 1 Epidemiological DO status. Values: 1 - YES; 0 - NO.
# STDNNOVA New DN model
# DNNOVA.CNV character 1 New DO status. Values: 1 - YES; 0 - NO.




# ## these are new starting in 2010 data
# NOMEPAI = list(
#   type     = "character",
#   name_en  = "f_name",
#   label_en = "Father's name",
#   label    = "Nome do pai"
# ),
# IDADEPAI = list(
#   type     = "character",
#   name_en  = "f_age",
#   label_en = "Father's age",
#   label    = "Idade do pai"
# ),
# DTULTMENST = list(
#   type     = "character",
#   name_en  = "menst_date",
#   label_en = "Date of last menstruation in in ddmmyyyy format",
#   map_en   = c(),
#   label    = "Data da última menstruação (DUM): dd mm aaaa",
#   map      = c()
# ),
# SEMAGESTAC = list(
#   type     = "character",
#   name_en  = "gage_weeks",
#   label_en = "Number of weeks of gestation",
#   label    = "Número de semanas de gestação"
# ),
# TPMETESTIM = list(
#   type     = "character",
#   name_en  = "gage_method",
#   label_en = "Method used (to estimate gestational age??)",
#   map_en   = c("1" = "Physical examination",
#                "2" = "Another method"),
#   label    = "Método utilizado",
#   map      = c("1" = "Exame físico",
#                "2" = "Outro método",
#                "9" = NA)
# ),
# CODCRM = list(
#   type     = "character",
#   name_en  = "crm_code",
#   label_en = "Physician's CRM code",
#   label    = "Código do CRM do médico"
# ),
# MESPRENAT = list(
#   type     = "character",
#   name_en  = "prenat_gmonth",
#   label_en = "Month of gestation in which the prenatal period began",
#   label    = "Mês de gestação em que iniciou o pré‐natal"
# ),
# TPAPRESENT = list(
#   type     = "character",
#   name_en  = "present_type",
#   label_en = "Type of presentation of the RN",
#   map_en   = c("1" = "Cephalic",
#                "2" = "Pelvic or breeding",
#                "3" = "Transverse",
#                "9" = NA),
#   label    = "Tipo de apresentação do RN",
#   map      = c("1" = "Cefálico",
#                "2" = "Pélvica ou podálica",
#                "3" = "Transversa",
#                "9" = NA)
# ),
# STTRABPART = list(
#   type     = "character",
#   name_en  = "induced",
#   label_en = "Induced labor?",
#   map_en   = c("1" = "Yes",
#                "2" = "No",
#                "3" = "Not applicable",
#                "9" = NA),
#   label    = "Trabalho de parto induzido?",
#   map      = c("1" = "Sim",
#                "2" = "Não",
#                "3" = "Não se aplica",
#                "9" = NA)
# ),
# STCESPARTO = list(
#   type     = "character",
#   name_en  = "",
#   label_en = "",
#   map_en   = c(),
#   label    = "Cesárea ocorreu antes do trabalho de parto iniciar?",
#   map      = c("1" = "Sim",
#                "2" = "Não",
#                "3" = "Não se aplica",
#                "9" = NA)
# ),
# TPNASCASSI = list(
#   type     = "character",
#   name_en  = "assist",
#   label_en = "Birth was assisted by?",
#   map_en   = c("1" = "Medical",
#                "2" = "Nurse / midwife",
#                "3" = "Midwife",
#                "4" = "Others",
#                "9" = "Ignored."),
#   label    = "Nascimento foi assistido por?",
#   map      = c("1" = "Médico",
#                "2" = "Enfermeira/obstetriz",
#                "3" = "Parteira",
#                "4" = "Outros",
#                "9" = NA)
# ),
# # QUESTION: obstetriz and obstetriz both translate as widwife
# NOMERNASC = list(
#   type     = "character",
#   name_en  = "n_name",
#   label_en = "Name of newborn",
#   label    = "Nome do recém‐nascido"
# ),
# NOMERESPO = list(
#   type     = "character",
#   name_en  = "ic_name",
#   label_en = "Name of person in charge",
#   label    = "Nome do responsável pelo preenchimento"
# ),
# TPFUNCRESP = list(
#   type     = "character",
#   name_en  = "ic_role",
#   label_en = "Role of the person in charge",
#   map_en   = c("1" = "Physician",
#                "2" = "Nurse",
#                "3" = "Midwife",
#                "4" = "Employee of the registry",
#                "5" = "Others"),
#   label    = "Tipo de função do responsável pelo preenchimento",
#   map      = c("1" = "Médico",
#                "2" = "Enfermeiro",
#                "3" = "Parteira",
#                "4" = "Funcionário do cartório",
#                "5" = "Outros")
# ),
# DSFUNCRESP = list(
#   type     = "character",
#   name_en  = "ic_role_desc",
#   label_en = "Description of the role of the person for 'ic_role' if 'Other' was specified",
#   label    = "Descrição da função do responsável pelo preenchimento, se a função for “5– Outros”"
# ),
# CODPROF = list(
#   type     = "character",
#   name_en  = "ic_role_prof",
#   label_en = "Code of the profession for 'ic_role' if 'Other' was specified",
#   label    = "Código da profissão, se a função for “5– Outros”"
# ),
# PROFISSION = list(
#   type     = "character",
#   name_en  = "ic_role_name",
#   label_en = "Name of the profession for 'ic_role' if 'Other' was specified",
#   label    = "Nome do Profissional, se a função for “5– Outros”"
# ),
# COREN = list(
#   type     = "character",
#   name_en  = "np_name",
#   label_en = "Name of nurse practitioner",
#   label    = "Nome do profissional enfermeiro"
# ),
# CODCOREN = list(
#   type     = "character",
#   name_en  = "np_code",
#   label_en = "Nurse practitioner code",
#   label    = "Código do COREN"
# ),
# TPDOCRESP = list(
#   type     = "character",
#   name_en  = "np_doc_type",
#   label_en = "Type of document of the person in charge",
#   map_en   = c("1" = "CNES",
#                "2" = "CRM",
#                "3" = " COREN",
#                "4" = "RG",
#                "5" = "CPF"),
#   label    = "Tipo do documento do responsável",
#   map      = c("1" = "CNES",
#                "2" = "CRM",
#                "3" = "COREN",
#                "4" = "RG",
#                "5" = "CPF")
# )
