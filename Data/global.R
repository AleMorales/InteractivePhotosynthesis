options(stringsAsFactors = FALSE)

# Check if all the dependencies are available. If not, install them
test = try(find.package("ggplot2"))
if(inherits(test,"try-error")) install.packages("ggplot2")
test = try(find.package("stringr"))
if(inherits(test,"try-error")) install.packages("stringr")
test = try(find.package("shinyBS"))
if(inherits(test,"try-error")) install.packages("shinyBS")
test = try(find.package("jsonlite"))
if(inherits(test,"try-error")) install.packages("jsonlite")

# Load Plotting module

source("plotModule.R",encoding = "UTF-8")
source("LabelsPlots.R",encoding = "UTF-8")
source("writeScenario.R",encoding = "UTF-8")

# C3 Photosynthesis -------------------------------------------------------

C3default = list(
  Sco25  =  2800,
  E_Sco  =  -24460,
  Kmc25  =  270,
  E_Kmc  =  80990,
  Kmo25  =  165000,
  E_Kmo  =  23720,
  theta  =  0.7,
  Phi2  =  0.83,
  sigma2  =  0.5,
  beta  =  0.85,
  fpseudo  =  0.1,
  fcyc  =  0.131,
  a1  =  0.9,
  b1  =  0.15,
  Vcmax25  =  120,
  E_Vcmax  =  65330,
  Jmax25  =  230,
  E_Jmax  =  30000,
  D_Jmax  =  2e+05,
  S_Jmax  =  650,
  Rd25  =  1.2,
  E_Rd  =  46390,
  gm25  =  0.4,
  E_gm  =  49600,
  D_gm  =  437400,
  S_gm  =  1400,
  gs0  =  0.01,
  gb  =  1.5,
  Ca  =  360,
  TPU25  =  12,
  E_TPU  =  53100,
  D_TPU  =  20180,
  S_TPU  =  650,
  VPD  =  2.1,
  O2  =  210,
  PAR  =  1000,
  Tleaf  =  25)

namesC3Parameters = names(C3default)
namesC3Parameters = namesC3Parameters[-which(namesC3Parameters %in% c("Ca", "VPD", "O2", "PAR", "Tleaf"))]

CalcAnC3 = function(gm,gs0,fvpd,gb,x2,x1,gamma_star,Rd,Ca) {
  a = gs0*(x2 + gamma_star) + (gs0/gm + fvpd)*(x1 - Rd)
  b = Ca*(x1 - Rd) - gamma_star*x1 - Rd*x2
  c = Ca + x2 + (1/gm + 1/gb)*(x1 - Rd)
  d = x2 + gamma_star + (x1 - Rd)/gm
  m = 1/gm + (gs0/gm + fvpd)*(1/gm + 1/gb)
  p = -(d + (x1 - Rd)/gm + a*(1/gm + 1/gb) + (gs0/gm + fvpd)*c)/m
  q = (d*(x1 - Rd) + a*c + (gs0/gm + fvpd)*b)/m
  r = (-a*b)/m
  U = (2*p^3 - 9*p*q + 27*r)/54.0
  Q = (p^2 - 3*q)/9.0
  psi = acos(U/sqrt(Q^3))
  A = -2*sqrt(Q)*cos(psi/3) - p/3
  return(A = A)
}

CalcCi = function(gs0, An, Ca, Ci_star, Rd, fvpd) {
  a = gs0
  b = An - gs0*Ca - gs0*Ci_star + (An + Rd)*fvpd
  c = -An*Ci_star + gs0*Ca*Ci_star - (An + Rd)*Ca*fvpd
  (-b + sqrt(b^2 - 4*a*c))/(2*a)
}


C3 = function(inputs = NULL) {
  Pair = 1.01e+08
  R =  8310
  T0 =  298.15
  Sco25 = inputs[[1]]*1
  E_Sco = inputs[[2]]*1000
  Kmc25 = inputs[[3]]*1e-06
  E_Kmc = inputs[[4]]*1000
  Kmo25 = inputs[[5]]*1e-06
  E_Kmo = inputs[[6]]*1000
  theta = inputs[[7]]*1
  Phi2 = inputs[[8]]*1
  sigma2 = inputs[[9]]*1
  beta = inputs[[10]]*1
  fpseudo = inputs[[11]]*1
  fcyc = inputs[[12]]*1
  a1 = inputs[[13]]*1
  b1 = inputs[[14]]*1e-06
  Vcmax25 = inputs[[15]]*1e-06
  E_Vcmax = inputs[[16]]*1000
  Jmax25 = inputs[[17]]*1e-06
  E_Jmax = inputs[[18]]*1000
  D_Jmax = inputs[[19]]*1000
  S_Jmax = inputs[[20]]*1000
  Rd25 = inputs[[21]]*1e-06
  E_Rd = inputs[[22]]*1000
  gm25 = inputs[[23]]*1
  E_gm = inputs[[24]]*1000
  D_gm = inputs[[25]]*1000
  S_gm = inputs[[26]]*1000
  gs0 = inputs[[27]]*1
  gb = inputs[[28]]*1
  Ca = inputs[[29]]*1e-06
  TPU25 = inputs[[30]]*1e-06
  E_TPU = inputs[[31]]*1000
  D_TPU = inputs[[32]]*1000
  S_TPU = inputs[[33]]*1000
  VPD = inputs[[34]]*1e+06
  O2 = inputs[[35]]*0.001
  PAR = inputs[[36]]*1e-06
  Tleaf = inputs[[37]]*1 + 273.15

  
  PAR[which(PAR == 0)] = 0 + .Machine$double.eps
  
  
  Rd = Rd25*exp(((Tleaf - T0)*E_Rd)/(T0*R*Tleaf))
  Vcmax = Vcmax25*exp(((Tleaf - T0)*E_Vcmax)/(T0*R*Tleaf))
  Kmc = Kmc25*exp(((Tleaf - T0)*E_Kmc)/(T0*R*Tleaf))
  Kmo = Kmo25*exp(((Tleaf - T0)*E_Kmo)/(T0*R*Tleaf))
  Sco = Sco25*exp(((Tleaf - T0)*E_Sco)/(T0*R*Tleaf))
  gamma_star = (0.5*O2)/Sco
  gm = (gm25*exp(((Tleaf - T0)*E_gm)/(T0*R*Tleaf))*(1 + exp((T0*S_gm - D_gm)/(R*T0))))/(1 + exp((Tleaf*S_gm - D_gm)/(R*Tleaf)))
  Ci_star = gamma_star - Rd/gm
  fvpd = pmax(1/(1/(a1 - b1*VPD) - 1),0)
  x1_c = Vcmax
  x2_c = Kmc*(1 + O2/Kmo)
  Jmax = (Jmax25*exp(((Tleaf - T0)*E_Jmax)/(T0*R*Tleaf))*(1 + exp((T0*S_Jmax - D_Jmax)/(R*T0))))/(1 + exp((Tleaf*S_Jmax - D_Jmax)/(R*Tleaf)))
  TPU = (TPU25*exp(((Tleaf - T0)*E_TPU)/(T0*R*Tleaf))*(1 + exp((T0*S_TPU - D_TPU)/(R*T0))))/(1 + exp((Tleaf*S_TPU - D_TPU)/(R*Tleaf)))
  k2ll = Phi2*sigma2*beta
  J = (k2ll*PAR + Jmax - sqrt((k2ll*PAR + Jmax)^2 - 4*theta*k2ll*Jmax*PAR))/(2*theta)
  x1_j = (J/4.0)*(1 - fpseudo/(1 - fcyc))
  x2_j = 2*gamma_star
  x1_p = 3.0*TPU
  x2_p = -gamma_star
  Ap = 3*TPU - Rd
  #Ccp = (3*TPU*x2_p + gamma_star*x1_p)/(x1_p - 3*TPU)

  
  Ac = CalcAnC3(gm, gs0, fvpd, gb, x2_c, x1_c, gamma_star, Rd, Ca)
  #Ccc = ((Ac + Rd)*x2_c + gamma_star*x1_c)/(x1_c - (Ac + Rd))
  Aj = CalcAnC3(gm, gs0, fvpd, gb, x2_j, x1_j, gamma_star, Rd, Ca)
  #Ccj = ((Aj + Rd)*x2_j + gamma_star*x1_j)/(x1_j - (Aj + Rd))

  An = ifelse(Ac < Aj & Ac < Ap, Ac, ifelse(Aj < Ap, Aj, Ap))
  Ag = An + Rd
  Ci = CalcCi(gs0, An, Ca, Ci_star, Rd, fvpd)
  Cc = Ci - An/gm
  #Cc = ifelse(Ac < Aj & Ac < Ap, Ccc, ifelse(Aj < Ap, Ccj, Ccp))
  #Ci = Cc + An/gm
  gs = gs0 + ((An + Rd)/(Ci - Ci_star))*fvpd
  Tr = (gs*1.56*VPD)/Pair
  WUE = An/Tr
  return(cbind(PAR = PAR/1e-6, Ca = Ca/1e-6, VPD = VPD/1e+06, O2 = O2/0.001, 
               An = An/1e-06, Ag = Ag/1e-06, gs = gs/1, Ci = Ci/1e-06, 
               Cc = Cc/1e-06, Tr = Tr*1000, WUE = WUE/0.001,
               Ac = Ac/1e-6, Aj = Aj/1e-6, Ap = Ap/1e-6,
               Tleaf = Tleaf - 273.15))
}

# C4 photosynthesis -------------------------------------------------------

C4default = list(
  Sco25 = 2590, 
  E_Sco = -24460, 
  Kmc25 = 650, 
  E_Kmc = 79430, 
  Kmo25 = 450000, 
  E_Kmo = 36380, 
  theta = 0.7, 
  Phi2 = 0.83, 
  sigma2 = 0.5, 
  beta = 0.85, 
  a1 = 0.9, 
  b1 = 0.15, 
  Vcmax25 = 120, 
  E_Vcmax = 65330, 
  x = 0.4, 
  alpha = 0.1, 
  Jmax25 = 230, 
  E_Jmax = 48000, 
  D_Jmax = 2e+05, 
  S_Jmax = 650, 
  fQ = 1, h = 3, 
  fpseudo = 0.1, 
  kp25 = 0.7, 
  E_kp = 46390, 
  Rd25 = 1.2, 
  E_Rd = 46390, 
  gbs = 0.003, 
  gs0 = 0.01, 
  gb = 1.5, 
  Ca = 360, 
  TPU25  =  20,
  E_TPU  =  53100,
  D_TPU  =  20180,
  S_TPU  =  650, 
  VPD = 2.1, 
  O2 = 210, 
  PAR = 1000, 
  Tleaf = 25)


namesC4Parameters = names(C4default)
namesC4Parameters = namesC4Parameters[-which(namesC4Parameters %in% c("Ca", "VPD", "O2", "PAR", "Tleaf"))]

CalcAnC4 = function(fvpd,gs0,gb,Ca,Cs_star,gamma_star,Rd,alpha,x3,x2,x1,gbs,Rm,O2,a,b) {
  pi = 3.14159265359
  d = gs0*Ca - gs0*Cs_star + fvpd*Rd
  m = fvpd - gs0/gb
  g = (b - Rm - gamma_star*O2*gbs)*x1*m - ((alpha*gamma_star)/0.047 + 1)*x1*d + a*gbs*x1*(Ca*m - d/gb - Ca + Cs_star)
  f = (b - Rm - gamma_star*O2*gbs)*x1*d + a*gbs*x1*Ca*d
  h = -(((alpha*gamma_star)/0.047 + 1)*x1*m + (a*gbs*x1*(m - 1))/gb)
  i = (b - Rm + gbs*x3 + x2*gbs*O2)*d + a*gbs*Ca*d
  j = (b - Rm + gbs*x3 + x2*gbs*O2)*m + ((alpha*x2)/0.047 - 1)*d + a*gbs*(Ca*m - d/gb - Ca + Cs_star)
  l = ((alpha*x2)/0.047 - 1)*m - (a*gbs*(m - 1))/gb
  p = (j - h + l*Rd)/l
  q = (i + j*Rd - g)/l
  r = -(f - i*Rd)/l
  Q = (p^2 - 3*q)/9
  U = (2*p^3 - 9*p*q + 27*r)/54
  Psi = acos(U/sqrt(Q^3))
  An = -2*sqrt(Q)*cos((Psi + 4*pi)/3) - p/3
  return(An)
}

C4 = function(inputs = NULL) {
  zeroumol = 0
  zeroconc =  0
  Pair = 1.01e+08
  R =  8310
  T0 =  298.15
  Sco25 = inputs[[1]]*1
  E_Sco = inputs[[2]]*1000
  Kmc25 = inputs[[3]]*1e-06
  E_Kmc = inputs[[4]]*1000
  Kmo25 = inputs[[5]]*1e-06
  E_Kmo = inputs[[6]]*1000
  theta = inputs[[7]]*1
  Phi2 = inputs[[8]]*1
  sigma2 = inputs[[9]]*1
  beta = inputs[[10]]*1
  a1 = inputs[[11]]*1
  b1 = inputs[[12]]*1e-06
  Vcmax25 = inputs[[13]]*1e-06
  E_Vcmax = inputs[[14]]*1000
  x = inputs[[15]]*1
  alpha = inputs[[16]]*1
  Jmax25 = inputs[[17]]*1e-06
  E_Jmax = inputs[[18]]*1000
  D_Jmax = inputs[[19]]*1000
  S_Jmax = inputs[[20]]*1000
  fQ = inputs[[21]]*1
  h = inputs[[22]]*1
  fpseudo = inputs[[23]]*1
  kp25 = inputs[[24]]*1
  E_kp = inputs[[25]]*1000
  Rd25 = inputs[[26]]*1e-06
  E_Rd = inputs[[27]]*1000
  gbs = inputs[[28]]*1
  gs0 = inputs[[29]]*1
  gb = inputs[[30]]*1
  Ca = inputs[[31]]*1e-06
  TPU25 = inputs[[32]]*1e-06
  E_TPU = inputs[[33]]*1000
  D_TPU = inputs[[34]]*1000
  S_TPU = inputs[[35]]*1000
  VPD = inputs[[36]]*1e+06
  O2 = inputs[[37]]*0.001
  PAR = inputs[[38]]*1e-06
  Tleaf = inputs[[39]]*1 + 273.15
  
  PAR[which(PAR == 0)] = 0 + .Machine$double.eps
  
  Rd = Rd25*exp(((Tleaf - T0)*E_Rd)/(T0*R*Tleaf))
  Rm = 0.5*Rd
  Sco = Sco25*exp(((Tleaf - T0)*E_Sco)/(T0*R*Tleaf))
  gamma_star = 0.5/Sco
  Vcmax = Vcmax25*exp(((Tleaf - T0)*E_Vcmax)/(T0*R*Tleaf))
  Kmc = Kmc25*exp(((Tleaf - T0)*E_Kmc)/(T0*R*Tleaf))
  Kmo = Kmo25*exp(((Tleaf - T0)*E_Kmo)/(T0*R*Tleaf))
  kp = kp25*exp(((Tleaf - T0)*E_kp)/(T0*R*Tleaf))
  x1_c1 = Vcmax
  x2_c1 = Kmc/Kmo
  x3_c1 = Kmc
  a_c1 = 1 + kp/gbs
  b_c1 = zeroumol
  Jmax = (Jmax25*exp(((Tleaf - T0)*E_Jmax)/(T0*R*Tleaf))*(1 + exp((T0*S_Jmax - D_Jmax)/(R*T0))))/(1 + exp((Tleaf*S_Jmax - D_Jmax)/(R*Tleaf)))
  TPU = (TPU25*exp(((Tleaf - T0)*E_TPU)/(T0*R*Tleaf))*(1 + exp((T0*S_TPU - D_TPU)/(R*T0))))/(1 + exp((Tleaf*S_TPU - D_TPU)/(R*Tleaf)))
  fcyc = 1 - (4*(1 - x)*(1 + fQ) + 3*h*fpseudo)/(3*h - 4*(1 - x))
  x1_c2 = Vcmax
  x2_c2 = Kmc/Kmo
  x3_c2 = Kmc
  x2_j1 = (7*gamma_star)/3
  x3_j1 = zeroconc
  a_j1 = 1 + kp/gbs
  b_j1 = zeroumol
  x2_j2 = (7*gamma_star)/3
  x3_j2 = zeroconc
  x1_p = 3.0*TPU
  x2_p = -gamma_star
  fvpd = pmax(1/(1/(a1 - b1*VPD) - 1),0)
  Ci_star = (gbs*gamma_star*O2 - Rd*(1 + (gamma_star*alpha)/0.047) + Rm)/(gbs + kp)
  Ap = 3*TPU - Rd
  k2ll = Phi2*sigma2*beta*(1 - fpseudo/(1 - fcyc))
  J = (k2ll*PAR + Jmax - sqrt((k2ll*PAR + Jmax)^2 - 4*theta*k2ll*Jmax*PAR))/(2*theta)
  z = (2 + fQ - fcyc)/(h*(1 - fcyc))
  x1_j1 = ((1 - x)*J*z)/3
  x1_j2 = ((1 - x)*J*z)/3
  Cs_star = Ci_star
  Ac1 = CalcAnC4(fvpd, gs0, gb, Ca, Cs_star, gamma_star, Rd, alpha, x3_c1, x2_c1, x1_c1, gbs, Rm, O2, a_c1, b_c1)
  Aj1 = CalcAnC4(fvpd, gs0, gb, Ca, Cs_star, gamma_star, Rd, alpha, x3_j1, x2_j1, x1_j1, gbs, Rm, O2, a_j1, b_j1)
  VpJ2 = (x*J*z)/2
  b_c2 = VpJ2
  b_j2 = VpJ2
  Ac2 = CalcAnC4(fvpd, gs0, gb, Ca, Cs_star, gamma_star, Rd, alpha, x3_c2, x2_c2, x1_c2, gbs, Rm, O2, 1, b_c2)
  Aj2 = CalcAnC4(fvpd, gs0, gb, Ca, Cs_star, gamma_star, Rd, alpha, x3_j2, x2_j2, x1_j2, gbs, Rm, O2, 1, b_j2)
  Ac = ifelse(Ac1 < Ac2, Ac1, Ac2)
  Aj = ifelse(Aj1 < Aj2, Aj1, Aj2)
  An = ifelse(Ac < Aj & Ac < Ap, Ac, ifelse(Aj < Ap, Aj, Ap))
  Ag = An + Rd
  
  gs = gs0 + fvpd*Ag/(Ca - An/gb - Cs_star)
  Ci = Ca - An*(1/gs + 1/gb)
  Vpep =  ifelse(An < min(Ac2,Aj2), kp*Ci, VpJ2) # pmin(kp*Ci, VpJ2)
  Cc = Ci + (Vpep - An - Rm)/gbs
  Tr = (gs*1.56*VPD)/Pair
  WUE = An/Tr
  
  return(cbind(PAR = PAR/1e-6, Ca = Ca/1e-6, VPD = VPD/1e+06, O2 = O2/0.001, 
               An = An/1e-06, Ag = Ag/1e-06, gs = gs/1, Ci = Ci/1e-06, 
               Cc = Cc/1e-06, Tr = Tr*1000, WUE = WUE/0.001,
               Ac = Ac/1e-6, Aj = Aj/1e-6, Ap = Ap/1e-6,
               Tleaf = Tleaf - 273.15))
}

# Description of parameters
descriptionParameters = list(
  Sco25  =  "Relative CO<sub>2</sub>/O<sub>2</sub> specificity factor for Rubisco at 25 &#176;C.",
  E_Sco  =  "Activation energy of Sco (J mol<sup>&minus;1</sup>).",
  Kmc25  =  "Michaelis-Menten of RuBP carboxylation for CO<sub>2</sub> at 25 &#176;C (&#956;mmol mol<sup>&minus;1</sup>).",
  E_Kmc  =  "Activation energy of Kmc (J mol<sup>&minus;1</sup>).",
  Kmo25  =  "Michaelis-Menten of RuBP oxygenation with respect to O<sub>2</sub> at 25 &#176;C (&#956;mmol mol<sup>&minus;1</sup>).",
  E_Kmo  =  "Activation energy of Kmo (J mol<sup>&minus;1</sup>).",
  theta  =  "Empirical parameter that determines the curvature of the light response of electron transport.",
  Phi2  =  "Maximum quantum yield of PS2 (dark-adapted leaves, equivalent to Fv/Fm).",
  sigma2  =  "Fraction of absorbed PAR that is associated to PSII.",
  beta  =  "Leaf-level light absorbance.",
  a1  =  "Empirical coefficient in VPD response of stomatal conductance.",
  b1  =  "Empirical coefficient in VPD response of stomatal conductance (kPa<sup>&minus;1</sup>).",
  Vcmax25  =  "Maximum rate of carboxylation (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  E_Vcmax  =  "Activation energy of Vcmax (J mol<sup>&minus;1</sup>).",
  x = "Fraction of electron transport at PSII partitioned to the C4 cycle.", 
  alpha = "Fraction of PSII activity in the bundle sheath.", 
  Jmax25  =  "Maximum rate of linear electron transport at 25 &#176;C (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  E_Jmax  =  "Activation energy of Jmax (J mol<sup>&minus;1</sup>).",
  D_Jmax  =  "Deactivation energy of Jmax (J mol<sup>&minus;1</sup>).",
  S_Jmax  =  "Entropy term in the temperature response of Jmax (J mol<sup>&minus;1</sup> K<sup>&minus;1</sup>).",
  fQ = "Fraction of electron transport at the cytochrome that goes through the Q-cycle.", 
  h = "Ratio of H<sup>+</sup> to ATP in the ATP synthase.", 
  fpseudo  =  "Fraction of electron transport at PSI that are used to reduce alternative electron sinks.",
  fcyc  =  "Fraction of electron transport at PSI that is cycled around PS1.",
  kp25 = "Initial carboxylation efficiency of the PEP carboxylase at 25 &#176;C (mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  E_kp = "Activation energy of kp (J mol<sup>&minus;1</sup>).",
  Rd25  =  "Rate of mitochondrial respiration at 25 &#176;C (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  E_Rd  =  "Activation energy of Rd (J mol<sup>&minus;1</sup>).",
  gbs = "Bundle-sheath conductance (mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  gs0  =  "Minimum stomatal conductance to fluxes of CO<sub>2</sub> (mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  gb  =  "Boundary layer conductance to flux of CO<sub>2</sub> (mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  TPU25  =  "Maximum rate of triose phosphate utilisation at 25 &#176;C (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  E_TPU  =  "Activation energy of TPU (J mol<sup>&minus;1</sup>).",
  D_TPU  =  "Deactivation energy of TPU (J mol<sup>&minus;1</sup>).",
  S_TPU  =  "Entropy term in the temperature response of TPU (J mol<sup>&minus;1</sup> K<sup>&minus;1</sup>).",
  gm25  =  "Mesophyll conductance to fluxes of CO<sub>2</sub> at 25 &#176;C.",
  E_gm  =  "Activation energy of gm (J mol<sup>&minus;1</sup>).",
  D_gm  =  "Deactivation energy of gm (J mol<sup>&minus;1</sup>).",
  S_gm  =  "Entropy term in the temperature response of gm (J mol<sup>&minus;1</sup> K<sup>&minus;1</sup>)."
)


# Description of environmental variables
descriptionEnvironment = list(
  Ca  =  "CO<sub>2</sub> molar fraction in the air (&#956;mol mol<sup>&minus;1</sup>).",
  VPD  =  "Leaf-to-air vapour pressure difference (kPa).",
  O2  =  "O<sub>2</sub> molar fraction in the air (mmol mol<sup>&minus;1</sup>).",
  PAR  =  "Photosynthetically active radiation  (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
  Tleaf  =  "Leaf temperature (&#176;C).")

# Description of outputs
descriptionOutput = list(
 An =  "Net CO<sub>2</sub> assimilation (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
 Ag =  "Gross CO<sub>2</sub> assimilation (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
 Ac =  "Net CO<sub>2</sub> assimilation lim. by Rubisco (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",
 Aj =  "Net CO<sub>2</sub> assimilation lim. by e<sup>&minus;</sup> transport (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).", 
 Ap =  "Net CO<sub>2</sub> assimilation lim. by TPU (&#956;mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).",  
 gs =  "Stomatal conductance (mol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).", 
 Ci =  "Intercellular CO<sub>2</sub> molar fraction (&#956;mol mol<sup>&minus;1</sup>).", 
 Cc =  "Chloroplast CO<sub>2</sub> molar fraction (&#956;mol mol<sup>&minus;1</sup>).", 
 Tr =  "Transpiration (mmol m<sup>&minus;2</sup> s<sup>&minus;1</sup>).", 
 WUE = "Water use efficiency (mmol mol<sup>&minus;1</sup>)."
) 

allLabels = c(descriptionEnvironment, descriptionOutput)

allVariables = names(c(RdescriptionEnvironment, RdescriptionOutput))

# Default values for environmental factors that are variable
curveVariables = list(PAR = seq(1,2000,25), 
                      Tleaf = seq(5,45,1), 
                      VPD = seq(0.5,3,0.5), 
                      Ca = seq(0,2000,25), 
                      O2 = seq(0,210,20))

# Lower bound for values for all parameters. 
# Parameters with NA for a value are dealt with in a special way (due to linear constraints on combination of values)

lowerParameters = c(
  Sco25  = 1,
  E_Sco  =  -1e5,
  Kmc25  =  1,
  E_Kmc  =  0,
  Kmo25  =  1,
  E_Kmo  =  0,
  theta  =  1e-4,
  Phi2  =  0,
  sigma2  =  0,
  beta  =  0,
  a1  =  NA,
  b1  =  NA,
  Vcmax25  =  0,
  E_Vcmax  =  0,
  x = 0,
  alpha = 0,
  Jmax25  =  0,
  E_Jmax  =  0,
  D_Jmax  =  0,
  S_Jmax  =  0,
  fQ = 0,
  h = 1,
  fpseudo  =  0,
  fcyc  =  0,
  kp25 = 0,
  E_kp = 0,
  Rd25  =  0,
  E_Rd  =  0,
  gbs = 1e-5,
  gs0  = 0,
  gb  =  1e-6,
  TPU25  =  0,
  E_TPU  =  0,
  D_TPU  =  0,
  S_TPU  =  0,
  gm25  =  1e-5,
  E_gm  =  0,
  D_gm  =  0,
  S_gm  = 0
)

# # Upper values for all parameters
# Parameters with NA for a value are dealt with in a special way (due to linear constraints on combination of values) 

upperParameters = c(
  Sco25  = 1e4,
  E_Sco  =  0,
  Kmc25  =  1e7,
  E_Kmc  =  1e6,
  Kmo25  =  1e8,
  E_Kmo  =  1e6,
  theta  =  1,
  Phi2  =  1,
  sigma2  =  1,
  beta  =  1,
  a1  =  NA,
  b1  =  NA,
  Vcmax25  =  1e6,
  E_Vcmax  =  6e5,
  x = 1,
  alpha = 1,
  Jmax25  =  1e6,
  E_Jmax  =  1e6,
  D_Jmax  =  1e6,
  S_Jmax  =  5e3,
  fQ = 1,
  h = 10,
  fpseudo  =  1,
  fcyc  =  0.99,
  kp25 = 1e3,
  E_kp = 1e6,
  Rd25  =  40,
  E_Rd  =  5e5,
  gbs = 1e6,
  gs0  =  1e6,
  gb  =  1e6,
  TPU25  =  1e6,
  E_TPU  =  5e5,
  D_TPU  =  1e6,
  S_TPU  =  5e3,
  gm25  =  1e6,
  E_gm  =  5e5,
  D_gm  =  1e7,
  S_gm  =  7e3
)


