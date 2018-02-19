#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List routeWaterLoop(int timeLength, CharacterVector edgeIDs, IntegerVector orders, NumericVector streamLengths, NumericVector streamWidths, NumericVector streamSlopes, NumericVector aCoeffs, NumericMatrix Rsurf, NumericMatrix Rsub, String by, List parentList, int spinUpYears, int spinUpCycles, double manningN, NumericVector vMonthConv, double beaverCoeff, NumericVector hillslopeLengths){
		
	Rcpp::Rcout << "Routing cpp"  << std::endl;
		
	//Create output matricies to store routing values in
	NumericMatrix MqOut(timeLength, edgeIDs.size());
	NumericMatrix Mv(timeLength, edgeIDs.size());
	NumericMatrix MsRiv(timeLength, edgeIDs.size());
	NumericMatrix Mh(timeLength, edgeIDs.size());
	NumericMatrix MsSub(timeLength, edgeIDs.size());
	NumericMatrix MqSub(timeLength, edgeIDs.size());

	//set counters used in looping
	int stepsLooped = 0;
	int cycleCount = spinUpCycles;
	int stepsPerYear = 365; 

	if(by == "month"){
		stepsPerYear = 12;
	}

	//NEED vConvFactor somewhere
	//manningN

	while(cycleCount <= spinUpCycles){

		int spinUpSteps = spinUpYears*stepsPerYear;
		int stepsToLoop = spinUpSteps;

		if(cycleCount >= spinUpCycles){
			stepsToLoop = timeLength;
			spinUpSteps = timeLength;
		}

			
		for(int timeStep=0; timeStep < stepsToLoop; timeStep++){

				double vConvFactor; 

				if(by == "month"){
					vConvFactor = vMonthConv[timeStep];
				} else {
					vConvFactor = 60*60*24/1000;
				}

			for(int i=0; i < edgeIDs.size(); i++){

				int ord = orders[i];

				double len = streamLengths[i];
				double width = streamWidths[i];


				double qIn = 0;

				if(ord > 0){
					NumericVector parents = parentList[i];

					if(parents.size() > 0){
						for(int j=0; j < parents.size(); j++){

							//the "-1" is to account for idice starting differences between c++ and R
							qIn += MqOut(timeStep, parents[j]-1);
						}
					}
				}



				double rS = Rsurf(timeStep, i);
				double rSub = Rsub(timeStep, i);

				double sRiv;
				double height;
				double sSub;

				if(timeStep == 0){
					// On the first timeStep, sRiv and sSub (storage) is 0, height needs to be initialized
					sRiv = MsRiv(spinUpSteps, i);
					height = Mh(spinUpSteps, i);
					sSub = MsSub(spinUpSteps, i);

				} else {

					// Set sRiv, sSub from previous day
					sRiv = MsRiv(timeStep-1, i);
					sSub = MsSub(timeStep-1, i);

					// Calculate height form sRiv and channel dimensions
					height = (sRiv * vConvFactor * 1000)/(len * 1000 * width);
				}

				// Manning's Eqation used to calculate velocity
				double v = (pow((height*width)/(2*height+width), (2.0/3.0)) * pow(streamSlopes[i], .5))/manningN;

				// Set velocity caps, upper cap may be unncessary, need to calibrate values to get realisic velocity
				if(v < .01){	
					v = .01;
				}

				// Convert velocity form m/s to km/timeStep (km/day)
				v = v*vConvFactor*beaverCoeff;

				// Caluclate groundwater discharge
				// Ignores current timeStep subsurface runoff, assuming that groundwater movement is too slow
				double qSub = pow((sSub+rSub)/aCoeffs[i], (1/.5));
			
				// Could base it off stream dimensions
				double qLoss = 0;
				
				//if(qIn > 1e5){
					//stop("Ridiculous Values!!!")
				//}
				
				// Assumes that l/v <= 1, if not, need different routing scheme 
				// Delta t is always one, so l/v can work

				double qOut;
				if(len/v < 1){
					qOut = sRiv + (1 - len/v)*qIn + (1 - len/(2*v))*(rS + qSub);
				} else {
					qOut = (v/len)*sRiv + 0*qIn + (v/(2*len))*(rS + qSub);
				}

				if(qOut != qOut){
					//Rcpp::Rcout << "Steps Looped: " << stepsLooped << std::endl;
					Rcpp::Rcout << "NaN Found" << std::endl;
					Rcpp::Rcout << "Timestep: " << timeStep << std::endl;
					Rcpp::Rcout << "Edge: " << i << std::endl;
					Rcpp::Rcout << "len: " << len << std::endl;
					Rcpp::Rcout << "v: " << v << std::endl;
					Rcpp::Rcout << "height: " << height << std::endl;
					Rcpp::Rcout << "width: " << width << std::endl;
					Rcpp::Rcout << "slope: " << streamSlopes[i] << std::endl;
					Rcpp::Rcout << "manningN: " << manningN << std::endl;
					//Rcpp::Rcout << "qOut: " << qOut << std::endl;
					//Rcpp::Rcout << "qIn: " << qIn << std::endl;
					//Rcpp::Rcout << "vConvFactor: " << vConvFactor << std::endl;
					Rcpp::Rcout << "Var: " << i << std::endl;
					Rcpp::Rcout << "Var: " << i << std::endl;
					Rcpp::Rcout << "Var: " << i << std::endl;
					Rcpp::Rcout << "Var: " << i << std::endl;

					return List::create(MqOut, MsRiv, MsSub, MqSub, Mv, Mh);
				}



				// Store values in M matricies
				MsRiv(timeStep, i) = sRiv + rS + qIn + qSub - qOut - qLoss;
				MsSub(timeStep, i) = sSub + rSub - qSub;
				MqOut(timeStep, i) = qOut;

				MqSub(timeStep, i) = qSub;
				Mv(timeStep, i) = v/vConvFactor;
				Mh(timeStep, i) = height;


			}

			stepsLooped++;


		}

		cycleCount++;
    }	

	Rcpp::Rcout << "Steps Looped: " << stepsLooped << std::endl;
	return List::create(MqOut, MsRiv, MsSub, MqSub, Mv, Mh);
}
