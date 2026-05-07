/*Création de la Table*/
data PM10;
    /* 1. Spécification du fichier source */
    infile "/home/u64437360/Projet PM10/PM10.dat" dlm='09'x dsd;
    
    /* 2. Lecture des variables */
    input 
          lg_conc_PM10 
          log_nbre_car_ph 
          tp_above_ground 
          wind_speed 
          tp_diff_bt_2_25 
          wind_direction 
          hour 
          day;
run;


/*Partie 1*/
PROC MEANS data=PM10 N NMISS MEAN STD MIN MAX; run;

proc univariate data=PM10 normal;
   var lg_conc_PM10 
          log_nbre_car_ph 
          tp_above_ground 
          wind_speed 
          tp_diff_bt_2_25 
          wind_direction 
          hour 
          day;
   histogram / normal kernel;
   inset n mean std skewness kurtosis / position=ne;
run;


proc fastclus data=PM10 maxclusters=2 out=clusters_vent;
   var wind_direction;
run;

data PM10_2;
    set PM10;
    
    length direction_cat $20;
    
    if (wind_direction >= 19 and wind_direction <= 127) then 
        direction_cat = "Axe_73";
    else if (wind_direction >= 149 and wind_direction <= 313) then 
        direction_cat = "Axe_231";
    else 
        direction_cat = "Vents_Variables";
run;    
quit;
proc glm data=PM10_2;
    class direction_cat;
    model lg_conc_PM10 = direction_cat;
    means direction_cat / HOVTEST welch; /* Teste si les moyennes sont différentes entre axes */
run;
quit;
data PM10_3;
    set PM10_2; 

    length periode_journee $20;
    
    /* 1. La Nuit (22h à 6h) : Camions et trafic nocturne */
    if (hour >= 22) or (hour <= 7) then 
        periode_journee = "1_Nuit";
        

    
    else 
        periode_journee = "2_Journee";
run;
proc glm data=PM10_3;
    /* On déclare la variable catégorielle */
    class periode_journee; 

    
    model lg_conc_PM10 = periode_journee 
                         log_nbre_car_ph
                       
                         periode_journee*log_nbre_car_ph 
                       
          / solution; 
run;
quit; 
data PM10_4;
    set PM10_3; /* On repart de ta table avec les périodes horaires */

    /* Création de la variable qualitative pour la stabilité de l'air */
    length stabilite_air $25;
    
    if tp_diff_bt_2_25 >= 0 then 
        stabilite_air = "1_Chaud_En_Haut";
    
    
    else if tp_diff_bt_2_25 < 0 then 
        stabilite_air = "2_Frais_En_Haut";
run;
proc ttest data=PM10_4;
    class stabilite_air; /* Ta variable explicative à 2 groupes */
    var lg_conc_PM10;    /* Ta variable à expliquer (quantitative) */
run;

proc corr data=PM10_4;
    
run;

/* Construction du modèle de régression simple */
proc reg data=PM10_4;
    model lg_conc_PM10 = log_nbre_car_ph ;
    
    ods select ParameterEstimates DiagnosticsPanel;
run;
quit;

proc glmselect data=PM10_4;
    /* On déclare les variables qualitatives */
    class periode_journee direction_cat stabilite_air; 

    /* On propose les effets principaux et les croisements stratégiques */
    model lg_conc_PM10 = wind_speed 
                         log_nbre_car_ph 
                         periode_journee 
                         stabilite_air
                         /* Croisement 1 : Trafic * Heure (Effet Camions) */
                         log_nbre_car_ph*periode_journee 
                         /* Croisement 2 : Trafic * Météo (Effet Blocage) */
                         log_nbre_car_ph*stabilite_air 
          / selection=stepwise(select=aic choose=bic);
run;







