#確認
source("../00_R_Script_gist/SPARQL_AllergroGraph_PropertyPath_v03.R")
source("../00_R_Script_gist/SPARQL_AllergroGraph_VisNet_v01.R")

AllergroPropertyPath_v2(ID_Name="wd:Q81163",
                                      PropertyPathDepth=1, Upper=T,
                                      Message=F,
                                      p1="rdfs:subClassOf",
                                      p2="rdfs:subClassOf",
                                      JA=T,
                                      FROM="FROM <http://nedo_jp> FROM <http://cw_2021_1_29_N> ",
                                      EndPoint="http://kozaki-lab.osakac.ac.jp/agraph/NEDO_pj") 

b <- AllergroPropertyPath_NEDOpj_up_v1(ID_Name = "wd:Q81163")

head(b)
Lab00 <- "重合体.wd:Q81163"
agVisNetwork(Graph=b, 
             NodeColorRandom=F, Count=2, 
             Size=10, SmallSize=5, StarSize=10, 
             FontSize=7, HeightSclale = "750px", 
             WidthSclale = "110%", SEED=123, Selected=Lab00)
