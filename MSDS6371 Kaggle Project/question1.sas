****************************;
* WITH OUTLIERS             ;
****************************;
PROC MEANS DATA=TRAIN1;
RUN;

DATA TRAIN2;
SET TRAIN1;
CENT1 = (GRLIVAREA_LOG - 13.0183)*(N1 - 0.2610966);
CENT2 = (GRLIVAREA_LOG - 13.0183)*(N2 - 0.1514360);

CENT1_LOG = (GRLIVAREA_LOG - 2.5141431)*(N1 - 0.2610966);
CENT2_LOG = (GRLIVAREA_LOG - 2.5141431)*(N2 - 0.1514360);

grcent=(GRLIVAREA_LOG - 2.5141431);

RUN;

ODS GRAPHICS ON;

SYMBOL1 V='SQUARE' C=BLACK I=NONE;
SYMBOL2 V='CIRCLE' C=RED I=NONE;
SYMBOL3 V='TRIANGLE' C=BLUE I=NONE;
TITLE 'House Sales Price and Sq Footage by Neighborhood';

*EDA AND REGRESSIONS WITHOUT TRANSFORMATIONS;
*PUT GRAPH IN REPORT;
PROC GPLOT DATA=TRAIN2;
PLOT SALEPRICE*GRLIVAREA=NEIGHBORHOOD;
RUN;

PROC REG DATA=TRAIN2;
MODEL SALEPRICE = N1 N2 GRLIVAREA /VIF; 
TITLE 'REGULAR';
RUN;

PROC REG DATA=TRAIN2;
MODEL SALEPRICE = N1 N2 GRLIVAREA INT1 INT2/VIF;
TITLE 'NO TRANSFORMATION WITH INTERACTION';
RUN;

PROC REG DATA=TRAIN2;
MODEL SALEPRICE = N1 N2 GRLIVAREA CENT1 CENT2/VIF;
TITLE 'NO TRANSFORMATIONS WITH CENTERING INTERACTION';
RUN;


*EDA AND REGRESSIONS WITH TRANSFORMATIONS;
* PUT GRAPH IN REPORT;
PROC GPLOT DATA=TRAIN2;
PLOT SALEPRICE_LOG*GRLIVAREA_LOG=NEIGHBORHOOD;
RUN;

PROC REG DATA=TRAIN2;
MODEL SALEPRICE_LOG = N1 N2 GRLIVAREA_LOG /VIF; 
TITLE 'LOG-LOG';
RUN;

PROC REG DATA=TRAIN2;
MODEL SALEPRICE_LOG = N1 N2 GRLIVAREA_LOG INT1_LOG INT2_LOG/VIF; 
TITLE 'LOG-LOG WITH INTERACTIONS';
RUN;

*THIS IS THE WINNING MODEL WITH OUTLIERS INCLUDED;
PROC REG DATA=TRAIN2 plots=all;
MODEL SALEPRICE_LOG = N1 N2 GRLIVAREA_LOG CENT1_LOG CENT2_LOG/VIF CLB CLM; 
TITLE 'LOG-LOG';
RUN;

*THIS IS THE GRAPH FOR THE WINNING MODEL WITH OUTLIERS INCLUDED;
PROC SGPLOT DATA=TRAIN2 ;
   TITLE "Sales Price by Living Area and Neighborhood";
   REG Y=SALEPRICE_LOG X=GRLIVAREA_LOG / GROUP=NEIGHBORHOOD ;
RUN;

****************************;
* WITHOUT OUTLIERS          ;
****************************;
DATA TRAIN_NO_OUTLIERS;
	SET TRAIN1;
	IF GRLIVAREA < 4000/100;
RUN;

PROC MEANS DATA=TRAIN_NO_OUTLIERS;
RUN;

DATA TRAIN_NO_OUTLIERS1;
	SET TRAIN_NO_OUTLIERS;
	CENT1 = (GRLIVAREA_LOG - 12.8158530)*(N1 - 0.2572178);
	CENT2 = (GRLIVAREA_LOG - 12.8158530)*(N2 - 0.1522310);

	CENT1_LOG = (GRLIVAREA_LOG - 2.5066639)*(N1 - 0.2572178);
	CENT2_LOG = (GRLIVAREA_LOG - 2.5066639)*(N2 - 0.1522310);
RUN;

SYMBOL1 V='SQUARE' C=BLACK I=NONE;
SYMBOL2 V='CIRCLE' C=RED I=NONE;
SYMBOL3 V='TRIANGLE' C=BLUE I=NONE;
TITLE 'House Sales Price and Sq Footage by Neighborhood';

PROC GPLOT DATA=TRAIN_NO_OUTLIERS1;
PLOT SALEPRICE_LOG*GRLIVAREA_LOG=NEIGHBORHOOD;
RUN;

****************************;
* THIS IS THE MODEL WE WANT ;
****************************;
PROC REG DATA=TRAIN_NO_OUTLIERS1 PLOTS=ALL;
MODEL SALEPRICE_LOG = N1 N2 GRLIVAREA_LOG CENT1_LOG CENT2_LOG/VIF CLB CLM; /*VIF SHOULD BE BELOW 10, IDEALLY CLOSE TO 1*/
TITLE 'LOG-LOG';
RUN;

*USE THIS GRAPH IN THE REPORT;
PROC SGPLOT DATA=TRAIN_NO_OUTLIERS1 ;
   TITLE "Sales Price by Living Area and Neighborhood";
   REG Y=SALEPRICE_LOG X=GRLIVAREA_LOG / GROUP=NEIGHBORHOOD ;
RUN;