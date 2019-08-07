DATA TRAIN2;
	SET TRAIN;

	SALEPRICE_LOG = LOG(SALEPRICE);

	IF GRLIVAREA < 4000;
	GRLIVAREA = GRLIVAREA/100;

	GRLIVAREA_LOG = LOG(GRLIVAREA);

	IF MSZONING = 'C (all)' THEN MSZONING1 = 0;
	ELSE IF MSZONING = 'FV' THEN MSZONING1 = 1;
	ELSE IF MSZONING = 'RH' THEN MSZONING1 = 2;
	ELSE IF MSZONING = 'RL' THEN MSZONING1 = 3;
	ELSE IF MSZONING = 'RM' THEN MSZONING1 = 4;

	IF STREET = 'Pave' THEN STREET1 = 0;
	ELSE IF STREET = 'Grvl' THEN STREET1 = 1;

	IF ALLEY = 'Grvl' THEN ALLEY1 = 0;
	ELSE IF ALLEY = 'Pave' THEN ALLEY1 = 1;
	ELSE IF ALLEY = 'NA' THEN ALLEY1=2;

	IF LOTSHAPE = 'IR1' THEN LOTSHAPE1 = 0;
	ELSE IF LOTSHAPE = 'IR2' THEN LOTSHAPE1 = 1;
	ELSE IF LOTSHAPE = 'IR3' THEN LOTSHAPE1 = 2;
	ELSE IF LOTSHAPE = 'Reg' THEN LOTSHAPE1 = 3;

	IF LANDCONTOUR = 'Bnk' THEN LANDCONTOUR1 = 0;
	ELSE IF LANDCONTOUR = 'HLS' THEN LANDCONTOUR1 = 1;
	ELSE IF LANDCONTOUR = 'Low' THEN LANDCONTOUR1 = 2;
	ELSE IF LANDCONTOUR = 'Lvl' THEN LANDCONTOUR1 = 3;

	IF UTILITIES = 'AllPub' THEN UTILITIES1 = 0;
	ELSE IF UTILITIES = 'NoSeWa' THEN UTILITIES1 = 1;

	IF LOTCONFIG = 'Corner' THEN LOTCONFIG1 = 0;
	ELSE IF LOTCONFIG = 'CulDSac' THEN LOTCONFIG1 = 1;
	ELSE IF LOTCONFIG = 'FR2' THEN LOTCONFIG1 = 2;
	ELSE IF LOTCONFIG = 'FR3' THEN LOTCONFIG1 = 3;
	ELSE IF LOTCONFIG = 'Inside' THEN LOTCONFIG1 = 4;

	IF LANDSLOPE = 'Gtl' THEN LANDSLOPE1 = 0;
	ELSE IF LANDSLOPE = 'Mod' THEN LANDSLOPE1 = 1;
	ELSE IF LANDSLOPE = 'Sev' THEN LANDSLOPE1 = 2;

	IF NEIGHBORHOOD = 'Blmngtn' THEN NEIGHBORHOOD0 = 0;
	ELSE IF NEIGHBORHOOD = 'Blueste' THEN NEIGHBORHOOD0 = 1;
	ELSE IF NEIGHBORHOOD = 'BrDale' THEN NEIGHBORHOOD0 = 2;
	ELSE IF NEIGHBORHOOD = 'BrkSide' THEN NEIGHBORHOOD0 = 3;
	ELSE IF NEIGHBORHOOD = 'ClearCr' THEN NEIGHBORHOOD0 = 4;
	ELSE IF NEIGHBORHOOD = 'CollgCr' THEN NEIGHBORHOOD0 = 5;
	ELSE IF NEIGHBORHOOD = 'Crawfor' THEN NEIGHBORHOOD0 = 6;
	ELSE IF NEIGHBORHOOD = 'Edwards' THEN NEIGHBORHOOD0 = 7;
	ELSE IF NEIGHBORHOOD = 'Gilbert' THEN NEIGHBORHOOD0 = 8;
	ELSE IF NEIGHBORHOOD = 'IDOTRR' THEN NEIGHBORHOOD0 = 9;
	ELSE IF NEIGHBORHOOD = 'MeadowV' THEN NEIGHBORHOOD0 = 10;
	ELSE IF NEIGHBORHOOD = 'Mitchel' THEN NEIGHBORHOOD0 = 11;
	ELSE IF NEIGHBORHOOD = 'NAmes' THEN NEIGHBORHOOD0 = 12;
	ELSE IF NEIGHBORHOOD = 'NoRidge' THEN NEIGHBORHOOD0 = 13;
	ELSE IF NEIGHBORHOOD = 'NPkVill' THEN NEIGHBORHOOD0 = 14;
	ELSE IF NEIGHBORHOOD = 'NridgHt' THEN NEIGHBORHOOD0 = 15;
	ELSE IF NEIGHBORHOOD = 'NWAmes' THEN NEIGHBORHOOD0 = 16;
	ELSE IF NEIGHBORHOOD = 'OldTown' THEN NEIGHBORHOOD0 = 17;
	ELSE IF NEIGHBORHOOD = 'Sawyer' THEN NEIGHBORHOOD0 = 18;
	ELSE IF NEIGHBORHOOD = 'SawyerW' THEN NEIGHBORHOOD0 = 19;
	ELSE IF NEIGHBORHOOD = 'Somerst' THEN NEIGHBORHOOD0 = 20;
	ELSE IF NEIGHBORHOOD = 'StoneBr' THEN NEIGHBORHOOD0 = 21;
	ELSE IF NEIGHBORHOOD = 'SWISU' THEN NEIGHBORHOOD0 = 22;
	ELSE IF NEIGHBORHOOD = 'Timber' THEN NEIGHBORHOOD0 = 23;
	ELSE IF NEIGHBORHOOD = 'Veenker' THEN NEIGHBORHOOD0 = 24;

	IF CONDITION1 = 'Artery' THEN CONDITION11 = 0;
	ELSE IF CONDITION1 = 'Feedr' THEN CONDITION11 = 1;
	ELSE IF CONDITION1 = 'Norm' THEN CONDITION11 = 2;
	ELSE IF CONDITION1 = 'PosA' THEN CONDITION11 = 3;
	ELSE IF CONDITION1 = 'PosN' THEN CONDITION11 = 4;
	ELSE IF CONDITION1 = 'RRAe' THEN CONDITION11 = 5;
	ELSE IF CONDITION1 = 'RRAn' THEN CONDITION11 = 6;
	ELSE IF CONDITION1 = 'RRNe' THEN CONDITION11 = 7;
	ELSE IF CONDITION1 = 'RRNn' THEN CONDITION11 = 8;

	IF CONDITION2 = 'Artery' THEN CONDITION21 = 0;
	ELSE IF CONDITION2 = 'Feedr' THEN CONDITION21 = 1;
	ELSE IF CONDITION2 = 'Norm' THEN CONDITION21 = 2;
	ELSE IF CONDITION2 = 'PosA' THEN CONDITION21 = 3;
	ELSE IF CONDITION2 = 'PosN' THEN CONDITION21 = 4;
	ELSE IF CONDITION2 = 'RRAe' THEN CONDITION21 = 5;
	ELSE IF CONDITION2 = 'RRAn' THEN CONDITION21 = 6;
	ELSE IF CONDITION2 = 'RRNn' THEN CONDITION21 = 7;

	IF BLDGTYPE = '1Fam' THEN BLDGTYPE1 = 0;
	ELSE IF BLDGTYPE = '2fmCon' THEN BLDGTYPE1 = 1;
	ELSE IF BLDGTYPE = 'Duplex' THEN BLDGTYPE1 = 2;
	ELSE IF BLDGTYPE = 'Twnhs' THEN BLDGTYPE1 = 3;
	ELSE IF BLDGTYPE = 'TwnhsE' THEN BLDGTYPE1 = 4;

	IF HOUSESTYLE = '1.5Fin' THEN HOUSESTYLE1 = 0;
	ELSE IF HOUSESTYLE = '1.5Unf' THEN HOUSESTYLE1 = 1;
	ELSE IF HOUSESTYLE = '1Story' THEN HOUSESTYLE1 = 2;
	ELSE IF HOUSESTYLE = '2.5Fin' THEN HOUSESTYLE1 = 3;
	ELSE IF HOUSESTYLE = '2.5Unf' THEN HOUSESTYLE1 = 4;
	ELSE IF HOUSESTYLE = '2Story' THEN HOUSESTYLE1 = 5;
	ELSE IF HOUSESTYLE = 'SFoyer' THEN HOUSESTYLE1 = 6;
	ELSE IF HOUSESTYLE = 'SLvl' THEN HOUSESTYLE1 = 7;

	IF ROOFSTYLE = 'Flat' THEN ROOFSTYLE1 = 0;
	ELSE IF ROOFSTYLE = 'Gable' THEN ROOFSTYLE1 = 1;
	ELSE IF ROOFSTYLE = 'Gambrel' THEN ROOFSTYLE1 = 2;
	ELSE IF ROOFSTYLE = 'Hip' THEN ROOFSTYLE1 = 3;
	ELSE IF ROOFSTYLE = 'Mansard' THEN ROOFSTYLE1 = 4;
	ELSE IF ROOFSTYLE = 'Shed' THEN ROOFSTYLE1 = 5;

	IF ROOFMATL = 'CompShg' THEN ROOFMATL1 = 0;
	ELSE IF ROOFMATL = 'Membran' THEN ROOFMATL1 = 1;
	ELSE IF ROOFMATL = 'Metal' THEN ROOFMATL1 = 2;
	ELSE IF ROOFMATL = 'Roll' THEN ROOFMATL1 = 3;
	ELSE IF ROOFMATL = 'Tar&Grv' THEN ROOFMATL1 = 4;
	ELSE IF ROOFMATL = 'WdShake' THEN ROOFMATL1 = 5;
	ELSE IF ROOFMATL = 'WdShngl' THEN ROOFMATL1 = 6;

	IF EXTERIOR1ST = 'VinylSd' THEN EXTERIOR1ST1 = 0;
	ELSE IF EXTERIOR1ST = 'MetalSd' THEN EXTERIOR1ST1 = 1;
	ELSE IF EXTERIOR1ST = 'Wd Sdng' THEN EXTERIOR1ST1 = 2;
	ELSE IF EXTERIOR1ST = 'HdBoard' THEN EXTERIOR1ST1 = 3;
	ELSE IF EXTERIOR1ST = 'BrkFace' THEN EXTERIOR1ST1 = 4;
	ELSE IF EXTERIOR1ST = 'WdShing' THEN EXTERIOR1ST1 = 5;
	ELSE IF EXTERIOR1ST = 'CemntBd' THEN EXTERIOR1ST1 = 6;
	ELSE IF EXTERIOR1ST = 'Plywood' THEN EXTERIOR1ST1 = 7;
	ELSE IF EXTERIOR1ST = 'AsbShng' THEN EXTERIOR1ST1 = 8;
	ELSE IF EXTERIOR1ST = 'Stucco' THEN EXTERIOR1ST1 = 9;
	ELSE IF EXTERIOR1ST = 'BrkComm' THEN EXTERIOR1ST1 = 10;
	ELSE IF EXTERIOR1ST = 'AsphShn' THEN EXTERIOR1ST1 = 11;
	ELSE IF EXTERIOR1ST = 'Stone' THEN EXTERIOR1ST1 = 12;
	ELSE IF EXTERIOR1ST = 'ImStucc' THEN EXTERIOR1ST1 = 13;
	ELSE IF EXTERIOR1ST = 'CBlock' THEN EXTERIOR1ST1 = 14;

	IF EXTERIOR2ND = 'VinylSd' THEN EXTERIOR2ND1 = 0;
	ELSE IF EXTERIOR2ND = 'MetalSd' THEN EXTERIOR2ND1 = 1;
	ELSE IF EXTERIOR2ND = 'Wd Shng' THEN EXTERIOR2ND1 = 2;
	ELSE IF EXTERIOR2ND = 'HdBoard' THEN EXTERIOR2ND1 = 3;
	ELSE IF EXTERIOR2ND = 'Plywood' THEN EXTERIOR2ND1 = 4;
	ELSE IF EXTERIOR2ND = 'Wd Sdng' THEN EXTERIOR2ND1 = 5;
	ELSE IF EXTERIOR2ND = 'CmentBd' THEN EXTERIOR2ND1 = 6;
	ELSE IF EXTERIOR2ND = 'BrkFace' THEN EXTERIOR2ND1 = 7;
	ELSE IF EXTERIOR2ND = 'AsbShng' THEN EXTERIOR2ND1 = 8;
	ELSE IF EXTERIOR2ND = 'Stucco' THEN EXTERIOR2ND1 = 9;
	ELSE IF EXTERIOR2ND = 'BrkComm' THEN EXTERIOR2ND1 = 10;
	ELSE IF EXTERIOR2ND = 'AsphShn' THEN EXTERIOR2ND1 = 11;
	ELSE IF EXTERIOR2ND = 'Stone' THEN EXTERIOR2ND1 = 12;
	ELSE IF EXTERIOR2ND = 'ImStucc' THEN EXTERIOR2ND1 = 13;
	ELSE IF EXTERIOR2ND = 'CBlock' THEN EXTERIOR2ND1 = 14;
	ELSE IF EXTERIOR2ND = 'Other' THEN EXTERIOR2ND1 = 15;

	IF MASVNRTYPE = 'BrkCmn' THEN MASVNRTYPE1 = 0;
	ELSE IF MASVNRTYPE = 'BrkFace' THEN MASVNRTYPE1 = 1;
	ELSE IF MASVNRTYPE = 'None' THEN MASVNRTYPE1 = 2;
	ELSE IF MASVNRTYPE = 'Stone' THEN MASVNRTYPE1 = 3;
	ELSE IF MASVNRTYPE = '' THEN MASVNRTYPE1 = 4;

	IF EXTERQUAL = 'Ex' THEN EXTERQUAL1 = 0;
	ELSE IF EXTERQUAL = 'Fa' THEN EXTERQUAL1 = 1;
	ELSE IF EXTERQUAL = 'Gd' THEN EXTERQUAL1 = 2;
	ELSE IF EXTERQUAL = 'TA' THEN EXTERQUAL1 = 3;

	IF EXTERCOND = 'Ex' THEN EXTERCOND1 = 0;
	ELSE IF EXTERCOND = 'Fa' THEN EXTERCOND1 = 1;
	ELSE IF EXTERCOND = 'Gd' THEN EXTERCOND1 = 2;
	ELSE IF EXTERCOND = 'Po' THEN EXTERCOND1 = 3;
	ELSE IF EXTERCOND = 'TA' THEN EXTERCOND1 = 4;

	IF FOUNDATION = 'BrkTil' THEN FOUNDATION1 = 0;
	ELSE IF FOUNDATION = 'CBlock' THEN FOUNDATION1 = 1;
	ELSE IF FOUNDATION = 'PConc' THEN FOUNDATION1 = 2;
	ELSE IF FOUNDATION = 'Slab' THEN FOUNDATION1 = 3;
	ELSE IF FOUNDATION = 'Stone' THEN FOUNDATION1 = 4;
	ELSE IF FOUNDATION = 'Wood' THEN FOUNDATION1 = 5;

	IF BSMTQUAL = 'Ex' THEN BSMTQUAL1 = 0;
	ELSE IF BSMTQUAL = 'Fa' THEN BSMTQUAL1 = 1;
	ELSE IF BSMTQUAL = 'Gd' THEN BSMTQUAL1 = 2;
	ELSE IF BSMTQUAL = 'NA' THEN BSMTQUAL1 = 3;
	ELSE IF BSMTQUAL = 'TA' THEN BSMTQUAL1 = 4;

	IF BSMTCOND = 'Fa' THEN BSMTCOND1 = 0;
	ELSE IF BSMTCOND = 'Gd' THEN BSMTCOND1 = 1;
	ELSE IF BSMTCOND = 'NA' THEN BSMTCOND1 = 2;
	ELSE IF BSMTCOND = 'Po' THEN BSMTCOND1 = 3;
	ELSE IF BSMTCOND = 'TA' THEN BSMTCOND1 = 4;

	IF BSMTEXPOSURE = 'Av' THEN BSMTEXPOSURE1 = 0;
	ELSE IF BSMTEXPOSURE = 'Gd' THEN BSMTEXPOSURE1 = 1;
	ELSE IF BSMTEXPOSURE = 'Mn' THEN BSMTEXPOSURE1 = 2;
	ELSE IF BSMTEXPOSURE = 'NA' THEN BSMTEXPOSURE1 = 3;
	ELSE IF BSMTEXPOSURE = 'No' THEN BSMTEXPOSURE1 = 4;

	IF BSMTFINTYPE1 = 'ALQ' THEN BSMTFINTYPE11 = 0;
	ELSE IF BSMTFINTYPE1 = 'BLQ' THEN BSMTFINTYPE11 = 1;
	ELSE IF BSMTFINTYPE1 = 'GLQ' THEN BSMTFINTYPE11 = 2;
	ELSE IF BSMTFINTYPE1 = 'LwQ' THEN BSMTFINTYPE11 = 3;
	ELSE IF BSMTFINTYPE1 = 'NA' THEN BSMTFINTYPE11 = 4;
	ELSE IF BSMTFINTYPE1 = 'Rec' THEN BSMTFINTYPE11 = 5;
	ELSE IF BSMTFINTYPE1 = 'Unf' THEN BSMTFINTYPE11 = 6;

	IF BSMTFINTYPE2 = 'ALQ' THEN BSMTFINTYPE21 = 0;
	ELSE IF BSMTFINTYPE2 = 'BLQ' THEN BSMTFINTYPE21 = 1;
	ELSE IF BSMTFINTYPE2 = 'GLQ' THEN BSMTFINTYPE21 = 2;
	ELSE IF BSMTFINTYPE2 = 'LwQ' THEN BSMTFINTYPE21 = 3;
	ELSE IF BSMTFINTYPE2 = 'NA' THEN BSMTFINTYPE21 = 4;
	ELSE IF BSMTFINTYPE2 = 'Rec' THEN BSMTFINTYPE21 = 5;
	ELSE IF BSMTFINTYPE2 = 'Unf' THEN BSMTFINTYPE21 = 6;

	IF HEATING = 'Floor' THEN HEATING1 = 0;
	ELSE IF HEATING = 'GasA' THEN HEATING1 = 1;
	ELSE IF HEATING = 'GasW' THEN HEATING1 = 2;
	ELSE IF HEATING = 'gRAV' THEN HEATING1 = 3;
	ELSE IF HEATING = 'OthW' THEN HEATING1 = 4;
	ELSE IF HEATING = 'Wall' THEN HEATING1 = 5;

	IF HEATINGQC = 'Ex' THEN HEATINGQC1 = 0;
	ELSE IF HEATINGQC = 'Fa' THEN HEATINGQC1 = 1;
	ELSE IF HEATINGQC = 'Gd' THEN HEATINGQC1 = 2;
	ELSE IF HEATINGQC = 'Po' THEN HEATINGQC1 = 3;
	ELSE IF HEATINGQC = 'TA' THEN HEATINGQC1 = 4;

	IF CENTRALAIR = 'N' THEN CENTRALAIR1 = 0;
	ELSE IF CENTRALAIR = 'Y' THEN CENTRALAIR1 = 1;

	IF ELECTRICAL = 'FuseA' THEN ELECTRICAL1 = 0;
	ELSE IF ELECTRICAL = 'FuseF' THEN ELECTRICAL1 = 1;
	ELSE IF ELECTRICAL = 'FuseP' THEN ELECTRICAL1 = 2;
	ELSE IF ELECTRICAL = 'Mix' THEN ELECTRICAL1 = 3;
	ELSE IF ELECTRICAL = 'SBrkr' THEN ELECTRICAL1 = 4;
	ELSE IF ELECTRICAL = '' THEN ELECTRICAL1 = 5;

	IF KITCHENQUAL = 'Ex' THEN KITCHENQUAL1 = 0;
	ELSE IF KITCHENQUAL = 'Fa' THEN KITCHENQUAL1 = 1;
	ELSE IF KITCHENQUAL = 'Gd' THEN KITCHENQUAL1 = 2;
	ELSE IF KITCHENQUAL = 'TA' THEN KITCHENQUAL1 = 3;

	IF FUNCTIONAL = 'Maj1' THEN FUNCTIONAL1 = 0;
	ELSE IF FUNCTIONAL = 'Maj2' THEN FUNCTIONAL1 = 1;
	ELSE IF FUNCTIONAL = 'Min1' THEN FUNCTIONAL1 = 2;
	ELSE IF FUNCTIONAL = 'Min2' THEN FUNCTIONAL1 = 3;
	ELSE IF FUNCTIONAL = 'Mod' THEN FUNCTIONAL1 = 4;
	ELSE IF FUNCTIONAL = 'Sev' THEN FUNCTIONAL1 = 5;
	ELSE IF FUNCTIONAL = 'Typ' THEN FUNCTIONAL1 = 6;

	IF FIREPLACEQU = 'Ex' THEN FIREPLACEQU = 0;
	ELSE IF FIREPLACEQU = 'Fa' THEN FIREPLACEQU = 1;
	ELSE IF FIREPLACEQU = 'Gd' THEN FIREPLACEQU = 2;
	ELSE IF FIREPLACEQU = 'NA' THEN FIREPLACEQU = 3;
	ELSE IF FIREPLACEQU = 'Po' THEN FIREPLACEQU = 4;
	ELSE IF FIREPLACEQU = 'TA' THEN FIREPLACEQU = 5;

	IF GARAGETYPE = '2Types' THEN GARAGETYPE1 = 0;
	ELSE IF GARAGETYPE = 'Attchd' THEN GARAGETYPE1 = 1;
	ELSE IF GARAGETYPE = 'Basement' THEN GARAGETYPE1 = 2;
	ELSE IF GARAGETYPE = 'BuiltIn' THEN GARAGETYPE1 = 3;
	ELSE IF GARAGETYPE = 'CarPort' THEN GARAGETYPE1 = 4;
	ELSE IF GARAGETYPE = 'Detchd' THEN GARAGETYPE1 = 5;
	ELSE IF GARAGETYPE = 'NA' THEN GARAGETYPE1 = 6;

	IF GARAGEFINISH = 'Fin' THEN GARAGEFINISH1 = 0;
	ELSE IF GARAGEFINISH = 'NA' THEN GARAGEFINISH1 = 1;
	ELSE IF GARAGEFINISH = 'RFn' THEN GARAGEFINISH1 = 2;
	ELSE IF GARAGEFINISH = 'Unf' THEN GARAGEFINISH1 = 3;

	IF GARAGEQUAL = 'Ex' THEN GARAGEQUAL1 = 0;
	ELSE IF GARAGEQUAL = 'Fa' THEN GARAGEQUAL1 = 1;
	ELSE IF GARAGEQUAL = 'Gd' THEN GARAGEQUAL1 = 2;
	ELSE IF GARAGEQUAL = 'NA' THEN GARAGEQUAL1 = 3;
	ELSE IF GARAGEQUAL = 'Po' THEN GARAGEQUAL1 = 4;
	ELSE IF GARAGEQUAL = 'TA' THEN GARAGEQUAL1 = 5;

	IF GARAGECOND = 'Ex' THEN GARAGECOND1 = 0;
	ELSE IF GARAGECOND = 'Fa' THEN GARAGECOND1 = 1;
	ELSE IF GARAGECOND = 'Gd' THEN GARAGECOND1 = 2;
	ELSE IF GARAGECOND = 'NA' THEN GARAGECOND1 = 3;
	ELSE IF GARAGECOND = 'Po' THEN GARAGECOND1 = 4;
	ELSE IF GARAGECOND = 'TA' THEN GARAGECOND1 = 5;

	IF PAVEDDRIVE = 'P' THEN PAVEDDRIVE1 = 0;
	ELSE IF PAVEDDRIVE = 'N' THEN PAVEDDRIVE1 = 1;
	ELSE IF PAVEDDRIVE = 'Y' THEN PAVEDDRIVE1 = 2;

	IF POOLQC = 'Ex' THEN POOLQC1 = 0;
	ELSE IF POOLQC = 'Fa' THEN POOLQC1 = 1;
	ELSE IF POOLQC = 'Gd' THEN POOLQC1 = 2;
	ELSE IF POOLQC = 'NA' THEN POOLQC1 = 3;

	IF FENCE = 'GdPrv' THEN FENCE1 = 0;
	ELSE IF FENCE = 'GdWo' THEN FENCE1 = 1;
	ELSE IF FENCE = 'MnPrv' THEN FENCE1 = 2;
	ELSE IF FENCE = 'MnWw' THEN FENCE1 = 3;
	ELSE IF FENCE = 'NA' THEN FENCE1 = 4;

	IF MISCFEATURE = 'Gar2' THEN MISCFEATURE1 = 0;
	ELSE IF MISCFEATURE = 'NA' THEN MISCFEATURE1 = 1;
	ELSE IF MISCFEATURE = 'Othr' THEN MISCFEATURE1 = 2;
	ELSE IF MISCFEATURE = 'Shed' THEN MISCFEATURE1 = 3;
	ELSE IF MISCFEATURE = 'TenC' THEN MISCFEATURE1 = 4;

	IF SALETYPE = 'COD' THEN SALETYPE1 = 0;
	ELSE IF SALETYPE = 'Con' THEN SALETYPE1 = 1;
	ELSE IF SALETYPE = 'ConLD' THEN SALETYPE1 = 2;
	ELSE IF SALETYPE = 'ConLI' THEN SALETYPE1 = 3;
	ELSE IF SALETYPE = 'ConLw' THEN SALETYPE1 = 4;
	ELSE IF SALETYPE = 'CWD' THEN SALETYPE1 = 5;
	ELSE IF SALETYPE = 'New' THEN SALETYPE1 = 6;
	ELSE IF SALETYPE = 'Oth' THEN SALETYPE1 = 7;
	ELSE IF SALETYPE = 'WD' THEN SALETYPE1 = 8;

	IF SALECONDITION = 'Abnorml' THEN SALECONDITION1 = 0;
	ELSE IF SALECONDITION = 'AdjLand' THEN SALECONDITION1 = 1;
	ELSE IF SALECONDITION = 'Alloca' THEN SALECONDITION1 = 2;
	ELSE IF SALECONDITION = 'Family' THEN SALECONDITION1 = 3;
	ELSE IF SALECONDITION = 'Normal' THEN SALECONDITION1 = 4;
	ELSE IF SALECONDITION = 'Partial' THEN SALECONDITION1 = 5;

	IF YEARBUILT <= 1949 AND YEARREMODADD = 1950 THEN YEARREMODADD1 = YEARBUILT;
	LOTFRONTAGE_LOG = LOG(LOTFRONTAGE);

RUN;

*https://blogs.sas.com/content/sasdummy/2013/06/12/correlations-matrix-heatmap-with-sas/;

%macro prepCorrData(in=,out=);
  /* Run corr matrix for input data, all numeric vars */
  proc corr data=&in. noprint
    pearson
    outp=work._tmpCorr
    vardef=df
  ;
  run;
 
  /* prep data for heat map */
data &out.;
  keep x y r;
  set work._tmpCorr(where=(_TYPE_="CORR"));
  array v{*} _numeric_;
  x = _NAME_;
  do i = dim(v) to 1 by -1;
    y = vname(v(i));
    r = v(i);
    /* creates a lower triangular matrix */
    if (i<_n_) then
      r=.;
    output;
  end;
run;
 
proc datasets lib=work nolist nowarn;
  delete _tmpcorr;
quit;
%mend;

ods path work.mystore(update) sashelp.tmplmst(read);
 
proc template;
  define statgraph corrHeatmap;
   dynamic _Title;
    begingraph;
      entrytitle _Title;
      rangeattrmap name='map';
      range -1 - 1 / rangecolormodel=(cxD8B365 cxF5F5F5 cx5AB4AC);
      endrangeattrmap;
      rangeattrvar var=r attrvar=r attrmap='map';
      layout overlay / 
        xaxisopts=(display=(line ticks tickvalues)) 
        yaxisopts=(display=(line ticks tickvalues));
        heatmapparm x = x y = y colorresponse = r / 
          xbinaxis=false ybinaxis=false
          name = "heatmap" display=all;
        continuouslegend "heatmap" / 
          orient = vertical location = outside title="Pearson Correlation";
      endlayout;
    endgraph;
  end;
run;

ods graphics /height=2400 width=2400 imagemap;

%prepCorrData(in=TRAIN2(drop=id) ,out=corr_matrix);
proc sgrender data=corr_matrix template=corrHeatmap;
   dynamic _title_="Corr Matrix";
run;

LOTFRONTAGE;
PROC GLMSELECT DATA=TRAIN2;
CLASS EXTERQUAL BSMTQUAL KITCHENQUAL GARAGEFINISH GARAGETYPE HEATINGQC BSMTEXPOSURE LOTSHAPE GARAGECOND CENTRALAIR FOUNDATION NEIGHBORHOOD;
MODEL SALEPRICE_LOG =  LOTAREA HALFBATH '2NDFLRSF'N WOODDECKSF OPENPORCHSF  FIREPLACES MASVNRAREA BSMTFINSF1 GARAGEYRBLT YEARBUILT 
	  TOTRMSABVGRD FULLBATH '1STFLRSF'N GARAGEAREA TOTALBSMTSF GARAGECARS GRLIVAREA_LOG OVERALLQUAL
	  | EXTERQUAL BSMTQUAL KITCHENQUAL GARAGEFINISH GARAGETYPE HEATINGQC BSMTEXPOSURE LOTSHAPE GARAGECOND CENTRALAIR FOUNDATION
  / SELECTION = FORWARD(STOP=CV) CVMETHOD=RANDOM(5) STATS=ADJRSQ;
RUN;


ODS GRAPHICS ON;
PROC GLM DATA=TRAIN2;
CLASS CENTRALAIR NEIGHBORHOOD;
MODEL SALEPRICE_LOG = GRLIVAREA_LOG BSMTFINSF1 YEARREMODADD TOTALBSMTSF LOTFRONTAGE GARAGECARS
	  | CENTRALAIR NEIGHBORHOOD / SOLUTION CLPARM;
RUN;

PROC SORT DATA=TRAIN2;
BY NEIGHBORHOOD;
RUN;

PROC REG DATA=TRAIN2;
BY NEIGHBORHOOD;
MODEL SALEPRICE_LOG = GRLIVAREA_LOG BSMTFINSF1 YEARREMODADD TOTALBSMTSF LOTFRONTAGE GARAGECARS;
RUN;

PROC SORT DATA=TRAIN2;
BY CENTRALAIR EXTERQUAL OVERALLQUAL;
RUN;

PROC SGSCATTER DATA=TRAIN2;
MATRIX SALEPRICE_LOG GRLIVAREA_LOG BSMTFINSF1 YEARREMODADD TOTALBSMTSF LOTAREA GARAGECARS;
RUN;
