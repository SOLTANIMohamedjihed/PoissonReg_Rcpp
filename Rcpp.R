library(Rcpp)
library(RcppArmadillo)

# la fonction de régression de Poisson en utilisant Rcpp
cppFunction('
  NumericVector regression_poisson(NumericMatrix X, NumericVector y, double alpha, int numIterations) {
    int m = X.nrow(); // Nombre déchantillons
    int n = X.ncol(); // Nombre de caractéristiques

    NumericVector theta(n, 0.0); // Initialise les paramètres à zéro

    // Effectue les itérations pour la descente de gradient
    for (int iter = 0; iter < numIterations; ++iter) {
      NumericVector predictions(m, 0.0); // Initialise les prédictions

      // Calcule les prédictions pour chaque échantillon
      for (int i = 0; i < m; ++i) {
        double z = 0.0;
        for (int l = 0; l < n; ++l) {
          z += theta[l] * X(i, l);
        }
        predictions[i] = exp(z);
      }

      // Met à jour les paramètres theta en utilisant la descente de gradient
      for (int l = 0; l < n; ++l) {
        double gradient = 0.0;
        for (int i = 0; i < m; ++i) {
          gradient += (predictions[i] - y[i]) * X(i, l);
        }
        theta[l] -= alpha * gradient / m;
      }
    }

    return theta;
  }
')

# Exemple d'utilisation
X <- matrix(c(1, 2, 2, 3, 3, 4, 4, 5), ncol = 2, byrow = TRUE)
y <- c(1, 2, 3, 4)

alpha <- 0.1 # Taux d'apprentissage
numIterations <- 1000 # Nombre d'itérations

theta <- regression_poisson(X, y, alpha, numIterations)

# Affiche les paramètres theta
print(theta)