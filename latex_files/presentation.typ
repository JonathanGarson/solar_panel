#align(center,text(17pt)[*Presentation*])

= Slide 1

*Research Question:* How does quality affect the transmission of tariffs on imported solar panels and how do tariffs affect the quality composition of imports?

- tariff a pour objectif augmenter les prix -> qualité peut affecter la transmission et donc l'effet du tariff 
- qualité change à cause des tariffs -> impact welfare des consommateurs, environment compétitif des importateurs
- prendre en compte la qualité permet de mieux comprendre l'effet du pass-throug, surtout dans les anti-dumping case ou la literature est divisée

= Slide 2

- anti-dumping literature : 
  - AD sont la pricincipales sources de transmission de tariffs au moins jusqu'à Trump -> 10% des biens chinois concernées aux U.S.
  - Literature divisée entre over pass-through and incomplete pass-through
  - Potentiallement augmentation de la qualité comme channel de l'augmentation de la productivité
  - Importance pour l'environment compétitif 

- Alchian-Allen : montre que on peut retrouver une forme d'effet Alchian-Allen avec une amélioration de la qualité induite par des tariffs add-valorem incomplete pass-through and corrélée à la qualité 
- Solar Industry Literature : 
  - peu de papier sur le sujet, Andres pour l'Europe n'adresse pas réellement la question de la qualité ou du pass-through mais de l'innovation
  - IO literature l'industrie solaire a reçu beaucoup d'attention mais peu sur le pass-through ou la qualité des biens

= Slide 3 
Theoretical Framework : pour guider notre analyse nous utilisons le modèle de Antoniades fondé sur un modèle Melitz & Ottaviano. Nous utilisons ce modèle pour les variables markups et le choix endogène de la qualité qui est important pour répondre à notre question

Empirical Framework : estimation du pass-through, du changement de la qualité, et de la demande pour tester si l'elasticité était plus faible pour des biens de hautes qualités 

= Slide 4
NA

= Slide 5
* Utility function *
Fonction d'utilité quasi-linéaire avec numéraire $q_0$, un quality shifter $z_i$, $alpha$ and $eta$ are the degree of subsituatibi;ity between the numéraire and the differentiated good, and $gamma$ the degree of horizontal differentation between varieties.

*Firm Cost* 
$c_i q_i$ represente le coût de fabrication marginal d'une unité supplémentaire, $theta$ est la difficulté à augmenter la qualité d'un bien. La convexité de la difficulté est importante pour rendre cela coûteux.

$delta$ est le coût de transport qui est directement intégré au coût de fabrication, ce qui confère aux entreprise plus productive un avantage comparatif à l'export.

= Slide 14
Year-Quarter x Origin FE absorb most of the price variance which happens at this level. Resulting in no effects.

= Slide 17

More details about UQE with FE :
- Step 1 : Estimate the location model $EE[X'(y - X beta)] = 0$ -> linear regression, covariates are uncorellated with the errro term
- Step 2 : Scale model $EE[X'(abs(y - X beta) - X gamma)] = 0$ -> absolute value of residual 
- Step 3 : Standardized Quantile : $EE[I((y - X beta)/(X gamma) >= q_tau) - tau] = 0$
- Step 4 : Aggregate $beta(tau) = beta + q_tau gamma$

The model assumes multiplicative heteroskedasticity linear in parameter: 

$
  y_i &= x'_i beta + nu_i \
  nu_i &= epsilon_i times x'_i
$

This is key to modelize the variance of the quantile regression, the scale $x'_i gamma$ determines how the variance of $y_i$ varies with $x'_i$. The scale is modelled as linear $x'_i gamma$.

Under the hypothesis that the error $epsilon$ is iid, it leads to :

$
  Q_y (tau | X) &= X beta + underbrace(Q_epsilon (tau), "Quantile Distribution of Error") times X gamma \
  => beta (tau) &= beta + q_tau times gamma
$

Location model estimation is a classical LR : 

$
  y_i = x'_i beta + nu_i "and" EE [x_i nu_i] = 0
$

Proof :

$
  EE[nu_i|x'_i] &= EE = [epsilon_i x'_i gamma | x'_i] = x'_i gamma underbrace(EE[epsilon_i | x_i], 0) = 0 \
  =>&   EE[x'_i nu] = EE[EE[x'_i nu]| x_i] = EE[x' underbrace(EE[nu|x'_i], 0)] = 0
$

Second step is that after the location, we need the scale coefficients which can identify due to the way we modelled the heteroskedasticity in our data as a linear function of X. For this we just use the absolute value of of the error term from the location model $u$ as a dependent variable which allows us to estimate the conditional standard deviation (rather than conditional variance) of the errors. Identification occurs if :

$
  abs(nu_i) &= x'_i gamma + omega_i \
  &EE[x_i (abs(nu_i) - x'_i gamma)] = 0
$

By regressing our absolute value of the residuals of the first regression this is similar to obtaining the conditional standard deviation on observables $x'_i$. In doing so, we assume that the error term $omega_i$ captures the iid part of the previous regression. So that we are left with $hat(gamma)$ being the estimated scale model.

$x'_i gamma$ must be otherwise coefficients might cross.

Third step, given the location and scale coefficients. The $tau_"th"$ quantile of error $epsilon$ can be estimated using the following condition:

$  y_i = x'_i beta + x'_i gamma times epsilon_i => epsilon_i = (y_i - x'_i beta)/(x'_i gamma) $ 

Standardize residuals provide an estimate of the latent error $epsilon_i$.

Therefore to estimate the $q_tau$ of the distribution of standard error, we just need to 

$
  EE [1 (x'_i (beta + gamma q_tau) >= y_i )- tau] &= 0 \
  EE [ 1(q_tau >= (y_i - x'_i beta)/(x'_i gamma) - tau )] &= 0
$

This is equivalent to ask "what value of $q_tau$ makes the proportion of observations where the standardized error is below $q_tau$" equal to $tau$. 

*Why my results are flat* 

Scale model : 
- maybe homoskedastic 
- linear scale model might not be appropriate
- covariate do not capture well heteroskedasticity
- standard regression quantile to check if they differ along $tau$

= Slide 19 

NegBin vs QuasiPoisson

Quasi Poisson :
$
  EE (Y) &= mu \
  "var"(Y) = nu_("poi") (mu) = theta mu
$

NegBin :

$
  EE(y) &= mu \
  "var"(y) = nu_("NB") (mu) = mu + kappa mu^2
$

List difference :
- variance: 
  - QuasiPoisson = variance is linear to the mean
  - Negbin = variance is quadritic to the mean
- weighting is different : 
  - to fit the model they use weighted least squares, and weights are inversely proportional to the variance.
    - QuasiPoisson = weight proportional to the mean 
    - NegBin = weights are concave to the mean, give less weight to small mean values 

= Appendix

*UQE* 