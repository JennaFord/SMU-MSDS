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

ods graphics /height=1200 width=1600 imagemap;

%prepCorrData(in=TRAIN_NO_OUTLIERS1(drop=id) ,out=corr_matrix);
proc sgrender data=corr_matrix template=corrHeatmap;
   dynamic _title_="Corr Matrix";
run;




/*proc corresp data=SASHELP.Cars all chi2p;*/
/*   tables Marital, Origin;*/
/*run;*/




PROC GLMSELECT DATA=TRAIN_NO_OUTLIERS1;
CLASS MSZoning Street;
MODEL SALEPRICE_LOG =  MSSubClass	MSZoning	LotFrontage	LotArea	Street GRLIVAREA_LOG
  / SELECTION = FORWARD(STOP=CV) CVMETHOD=RANDOM(5) STATS=ADJRSQ;
RUN;