proc logistic data=case.bonvend plots=roc;
	qualimodfin2: model bon_vendeur_AUG_FL(event='1')= /*NHML*/ HML /*temps_partiel*/
	temps_plein /*janvier*/ fevrier mars avril mai juin juillet aout septembre octobre
	novembre decembre /*Moins_de_15__de_taux_de_retrait*/ Entre_15_et_20__de_taux_de_retra
	Plus_de_20__de_taux_de_retrait /*Moins de 1 min sortants*/ Entre_1_et_1_5_min_moyen_sortant
	Plus_de_1_5_min_moyen_sortants /*Moins de 2 min entrants*/ Entre_2_et_2_5_min_moyen_entrant
	Entre_2_5_min_et_3_min_moyen_ent Plus_de_3_min_moyen_entrants /*Moins_de_50__d_appels_entrants*/
	var26 var27 Plus_de_65__d_appels_entrants 
	/*Moins de 95 taux de prise*/Plus_de_95__de_taux_de_prise_d_a
	g1 g2 g3 /*g4*//link=probit ctable /*pprob=0.52*/;
	test HML=0;/*1*/
	test temps_plein=0;/*2*/
	test fevrier=mars=avril=mai=juin=juillet=aout=septembre=octobre=novembre=decembre=0;/*3*/
	test Plus_de_95__de_taux_de_prise_d_a=0;/*4 Non significatif à 5%*/
	test var26=var27=Plus_de_65__d_appels_entrants=0;/*5 Non significatif à 5%*/
	test Entre_15_et_20__de_taux_de_retra=Plus_de_20__de_taux_de_retrait=0;/*6 Non significatif à 5%*/
	test Entre_1_et_1_5_min_moyen_sortant=Plus_de_1_5_min_moyen_sortants=0;/*7*/
	test Entre_2_et_2_5_min_moyen_entrant=Entre_2_5_min_et_3_min_moyen_ent=Plus_de_3_min_moyen_entrants=0;/*8*/
	test g2=g3=g1=0 /*9*/;
	OUTPUT OUT=resultfin2 predprobs=individuals;
run;
***Matrice de confusion;
proc freq data=resultfin2;
	table _into_ *_from_;
run;

/****Effets marginaux****/
***Macros;
%include "/folders/myfolders/sasuser.v94/fusion_63038_16_margins.sas";
run;
	%Margins(data= case,
               response = remiss, 
               roptions = event='1',
               model    = blast smear,
               dist     = binomial,
               effect   = blast,
               options  = cl)