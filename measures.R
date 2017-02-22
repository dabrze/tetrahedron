measureList <- list(
  "zeros" = function(a,b,c,d,alpha=1,beta=1) {matrix(0, length(a), 1)},
  "ones" = function(a,b,c,d,alpha=1,beta=1) {matrix(1, length(a), 1)},
  "Euclidean distance from tetrahedron center" = function(a,b,c,d,alpha=1,beta=1) {{
    result <- matrix(0, length(a), 1)
    for (q in 1:length(a)) {
      result[q] <- norm(cbind(a[q]-b[q]-c[q]+d[q], a[q]+b[q]-c[q]-d[q], a[q]-b[q]+c[q]-d[q]), "F")
    }
    result
  }},
  "a" = function(a,b,c,d,alpha=1,beta=1) {a},
  "b" = function(a,b,c,d,alpha=1,beta=1) {b},
  "c" = function(a,b,c,d,alpha=1,beta=1) {c},
  "d" = function(a,b,c,d,alpha=1,beta=1) {d},
  "a/n" = function(a,b,c,d,alpha=1,beta=1) {a / (a+b+c+d)},
  "b/n" = function(a,b,c,d,alpha=1,beta=1) {b / (a+b+c+d)},
  "c/n" = function(a,b,c,d,alpha=1,beta=1) {c / (a+b+c+d)},
  "d/n" = function(a,b,c,d,alpha=1,beta=1) {d / (a+b+c+d)},
  "(a+b)/n" = function(a,b,c,d,alpha=1,beta=1) {(a+b) / (a+b+c+d)},
  "(a+c)/n" = function(a,b,c,d,alpha=1,beta=1) {(a+c) / (a+b+c+d)},
  "(a+d)/n" = function(a,b,c,d,alpha=1,beta=1) {(a+d) / (a+b+c+d)},
  "(a+b+c)/n" = function(a,b,c,d,alpha=1,beta=1) {(a+b+c) / (a+b+c+d)},
  "(a+b+d)/n" = function(a,b,c,d,alpha=1,beta=1) {(a+b+d) / (a+b+c+d)},
  "(a+c+d)/n" = function(a,b,c,d,alpha=1,beta=1) {(a+c+d) / (a+b+c+d)},
  "(b+c+d)/n" = function(a,b,c,d,alpha=1,beta=1) {(b+c+d) / (a+b+c+d)},
  "(a-b)/n" = function(a,b,c,d,alpha=1,beta=1) {(a-b) / (a+b+c+d)},
  "(a-c)/n" = function(a,b,c,d,alpha=1,beta=1) {(a-c) / (a+b+c+d)},
  "(a-d)/n" = function(a,b,c,d,alpha=1,beta=1) {(a-d) / (a+b+c+d)},
  "(a+c)-(b+d)" = function(a,b,c,d,alpha=1,beta=1) {(a+c)-(b+d)},
  "(b+d)-(a+c)" = function(a,b,c,d,alpha=1,beta=1) {(b+d)-(a+c)},
  "a/(a+b+c)" = function(a,b,c,d,alpha=1,beta=1) {a / (a+b+c)},
  "b/(a+b+c)" = function(a,b,c,d,alpha=1,beta=1) {b / (a+b+c)},
  "c/(a+b+c)" = function(a,b,c,d,alpha=1,beta=1) {c / (a+b+c)},
  "d/(a+b+c)" = function(a,b,c,d,alpha=1,beta=1) {d / (a+b+c)},
  "True positive rate" = function(a,b,c,d,alpha=1,beta=1) {a / (a+c)},
  "Sensitivity" = function(a,b,c,d,alpha=1,beta=1) { measureList[["True positive rate"]](a,b,c,d,alpha,beta) },
  "Recall" = function(a,b,c,d,alpha=1,beta=1) { measureList[["True positive rate"]](a,b,c,d,alpha,beta) },
  "False negative rate" = function(a,b,c,d,alpha=1,beta=1) {c / (c+a)},
  "False positive rate" = function(a,b,c,d,alpha=1,beta=1) {b / (b+d)},
  "True negative rate" = function(a,b,c,d,alpha=1,beta=1) {d / (d+b)},
  "a/(a+b)" = function(a,b,c,d,alpha=1,beta=1) {a / (a+b)},
  "b/(a+b)" = function(a,b,c,d,alpha=1,beta=1) {b / (a+b)},
  "c/(c+d)" = function(a,b,c,d,alpha=1,beta=1) {c / (c+d)},
  "d/(c+d)" = function(a,b,c,d,alpha=1,beta=1) {d / (c+d)},
  "a/(a+c)" = function(a,b,c,d,alpha=1,beta=1) {a / (a+c)},
  "Accuracy" = function(a,b,c,d,alpha=1,beta=1) {(a+d) / (a+b+c+d)},
  "Balanced accuracy" = function(a,b,c,d,alpha=1,beta=1) {( a/(a+c) + d/(b+d) )/2},
  "Area Under Lift" = function(a,b,c,d,alpha=1,beta=1) {
    B = ( a/(a+c) + d/(b+d) )/2
    P = (a+c) / (a+b+c+d)
    P/2 + (1-P) * B
  },
  "Pointwise AUC-ROC" = function(a,b,c,d,alpha=1,beta=1) {a/(a+c) * d/(b+d)},
  "Optimized precision" = function(a,b,c,d,alpha=1,beta=1) {(a+d)/(a+b+c+d) - abs(d/(b+d) - a/(a+c)) / (d/(b+d) + a/(a+c))},
  "IBA(G-mean)" = function(a,b,c,d,alpha=0.5,beta=1) {
    alpha = 1/2
    Dom = a/(a+c) - d/(b+d)
    (1 + alpha*Dom) * ( (a/(a+c) * d/(b+d))^0.5 )
  },
  "IBA(F1 score)" = function(a,b,c,d,alpha=1,beta=1) {
    Dom = a/(a+c) - d/(b+d)

    precision = a/(a+b)
    recall = a/(a+c)
    (1 + alpha*Dom) * ( (1+1*1)*recall*precision / (1*1*precision + recall) )
  },
  "IBA(Fb-measure)" = function(a,b,c,d,alpha=1,beta=1) {
    Dom = a/(a+c) - d/(b+d)
    
    precision = a/(a+b)
    recall = a/(a+c)
    (1 + alpha*Dom) * ( (1+beta*beta)*recall*precision / (beta*beta*precision + recall) )
  },
  "IBA(Accuracy)" = function(a,b,c,d,alpha=0.25,beta=1) {
    Dom = a/(a+c) - d/(b+d)
    (1 + alpha*Dom) * ( (a+d) / (a+b+c+d) )
  },
  "Precision" = function(a,b,c,d,alpha=1,beta=1) {a / (a+b)},
  "Negative predictive value" = function(a,b,c,d,alpha=1,beta=1) {d / (c+d)},
  "Positive predictive value" = function(a,b,c,d,alpha=1,beta=1) { measureList[["Precision"]](a,b,c,d,alpha,beta) },
  "Specificity" = function(a,b,c,d,alpha=1,beta=1) {d / (b+d)},
  "Fb-measure" = function(a,b,c,d,alpha=1,beta=1) {
    precision = a/(a+b)
    recall = a/(a+c)
    (1+beta*beta)*recall*precision / (beta*beta*precision + recall)
  },
  "F1 score" = function(a,b,c,d,alpha=1,beta=1) {
    precision = a/(a+b)
    recall = a/(a+c)
    (1+1*1)*recall*precision / (1*1*precision + recall)
  },
  "Jaccard coefficient" = function(a,b,c,d,alpha=1,beta=1) {a / (a+b+c) },
  "G-mean" = function(a,b,c,d,alpha=1,beta=1) {
    TPR = a / (a+c)
    TNR = d / (d+b)
    sqrt(TPR*TNR)
  },
  "Sign of the determinant of the matrix" = function(a,b,c,d,alpha=1,beta=1) {sign(a*d - b*c)},
  "Certainty Factor" = function(a,b,c,d,alpha=1,beta=1) {
    result <- matrix(0, length(a), 1)
    cnf <- confirmation(a,b,c,d)
    zer <- neutral(a,b,c,d)
    dis <- disconfirmation(a,b,c,d)
    
    for (q in 1:length(a)) {
      if (all(c+d == 0)) {
        result[q] = +1
      }
      if (cnf[q]) {
        result[q] = (a[q]/(a[q]+c[q]) - (a[q]+b[q])/(a[q]+b[q]+c[q]+d[q]))/((c[q]+d[q])/(a[q]+b[q]+c[q]+d[q]))
      }
      if (zer[q]) {
        result[q] =  0
      }
      if (dis[q]) {
        result[q] = -((a[q]+b[q])/(a[q]+b[q]+c[q]+d[q]) - a[q]/(a[q]+c[q]))/((a[q]+b[q])/(a[q]+b[q]+c[q]+d[q]))
      }
      if (all(a+b == 0)){
        result[q] = -1
      }
    }
    
    result
  },
  "Diagnostic odds ratio" = function(a,b,c,d,alpha=1,beta=1) {(a * d) / (c * b)},
  "Log odds-ratio" = function(a,b,c,d,alpha=1,beta=1) {log(measureList[["Diagnostic odds ratio"]](a,b,c,d,alpha,beta))},
  "Kappa" = function(a,b,c,d,alpha=1,beta=1) {
    row_sum = rowSums(cbind(a,b,c,d))
   ((row_sum*a + row_sum*d  - (a+b)*(a+c) - (b+d)*(c+d)) / (row_sum*row_sum  - (a+b)*(a+c) - (b+d)*(c+d)))
  },
  "Lift" = function(a,b,c,d,alpha=1,beta=1) {
    row_sum = rowSums(cbind(a,b,c,d))
    (row_sum * a) / ((a+c)*(a+b))
  },
  "Log lift" = function(a,b,c,d,alpha=1,beta=1) {
    log(measureList[["Lift"]](a,b,c,d,alpha,beta))
  },
  "Piatetsky-Shapiro" = function(a,b,c,d,alpha=1,beta=1) {
    row_sum = rowSums(cbind(a,b,c,d))
    a / row_sum - ((a+c)*(a+b)) / row_sum^2
  },
  "Collective-strength" = function(a,b,c,d,alpha=1,beta=1) {
    row_sum = rowSums(cbind(a,b,c,d))
    ((a+d)/((a+c)*(a+b)*(b+d)*(c+d))) * ((row_sum^2 - (a+b)*(a+c) - (b+d)*(c+d))/(row_sum - a - d))
  },
  "All-confidence" = function(a,b,c,d,alpha=1,beta=1) {apply(cbind(a / (a+c), a / (a+b)), 1, min)},
  "Imbalance-ratio" = function(a,b,c,d,alpha=1,beta=1) {abs(c - b) / (a + c + b)},
  "D" = function(a,b,c,d,alpha=1,beta=1) {a / (a+c)  -  (a+b) / (a+b+c+d)},
  "M" = function(a,b,c,d,alpha=1,beta=1) {a / (a+b)  -  (a+c) / (a+b+c+d)},
  "S" = function(a,b,c,d,alpha=1,beta=1) {a / (a+c)  -  b / (b+d)},
  "N" = function(a,b,c,d,alpha=1,beta=1) {a / (a+b)  -  c / (c+d)},
  "C" = function(a,b,c,d,alpha=1,beta=1) {4 * (a*d - b*c) / ( (a + c + b + d) )^2},
  "F" = function(a,b,c,d,alpha=1,beta=1) {(a*d - b*c) / (a * d + b * c + 2 * a * c)},
  "Z" = function(a,b,c,d,alpha=1,beta=1) {
    result <- NaN*matrix(0, length(a), 1)
    cnf <- confirmation(a,b,c,d)
    zer <- neutral(a,b,c,d)
    dis <- disconfirmation(a,b,c,d)
    
    result[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + c[cnf]) * (c[cnf] + d[cnf]) )
    result[zer] = 0
    result[dis] = (a[dis]*d[dis] - b[dis]*c[dis])  / ( (a[dis] + c[dis]) * (a[dis] + b[dis]))
    
    result
  },
  "A" = function(a,b,c,d,alpha=1,beta=1) {
    result <- NaN*matrix(0, length(a), 1)
    cnf <- confirmation(a,b,c,d)
    zer <- neutral(a,b,c,d)
    dis <- disconfirmation(a,b,c,d)
    
    result[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + b[cnf]) * (b[cnf] + d[cnf]) )
    result[zer] = 0
    result[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (b[dis] + d[dis]) * (c[dis] + d[dis]) )
    
    result
  },
  "c1" = function(a,b,c,d,alpha=0.5,beta=1) {
    result <- NaN*matrix(0, length(a), 1)
    cnf <- confirmation(a,b,c,d)
    zer <- neutral(a,b,c,d)
    dis <- disconfirmation(a,b,c,d)
    
    Z = NaN*matrix(0, length(a), 1)
    Z[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + c[cnf]) * (c[cnf] + d[cnf]) )
    Z[zer] = 0
    Z[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (a[dis] + c[dis]) * (a[dis] + b[dis]) )
    
    A = NaN*matrix(0, length(a), 1)
    A[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + b[cnf]) * (b[cnf] + d[cnf]) )
    A[zer] = 0;
    A[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (b[dis] + d[dis]) * (c[dis] + d[dis]) )
  
    idx = (cnf & (c == 0))
    result[idx] =  alpha + (1-alpha)*A[idx]
    idx = (cnf & (c > 0))
    result[idx] = alpha*Z[idx]
    
    result[zer] = 0;
    
    idx = (dis & (a > 0))
    result[idx] = alpha*Z[idx]
    idx = (dis & (a == 0));
    result[idx] = -alpha + (1-alpha)*A[idx]
    
    result
  },
  "c2" = function(a,b,c,d,alpha=0.5,beta=1) {
    result <- NaN*matrix(0, length(a), 1)
    cnf <- confirmation(a,b,c,d)
    zer <- neutral(a,b,c,d)
    dis <- disconfirmation(a,b,c,d)
    
    Z = NaN*matrix(0, length(a), 1)
    Z[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + c[cnf]) * (c[cnf] + d[cnf]) )
    Z[zer] = 0
    Z[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (a[dis] + c[dis]) * (a[dis] + b[dis]) )
    
    A = NaN*matrix(0, length(a), 1)
    A[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + b[cnf]) * (b[cnf] + d[cnf]) )
    A[zer] = 0;
    A[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (b[dis] + d[dis]) * (c[dis] + d[dis]) )
    
    idx = (cnf & (b == 0))
    result[idx] =  alpha + (1-alpha)*Z[idx]
    idx = (cnf & (b > 0))
    result[idx] = alpha*A[idx]
    result[zer] = 0
    idx = (dis & (d > 0))
    result[idx] = alpha*A[idx];
    idx = (dis & (d == 0));
    result[idx] = -alpha + (1-alpha)*Z[idx]
    
    result
  },
  "c3" = function(a,b,c,d,alpha=1,beta=1) {
    result <- NaN*matrix(0, length(a), 1)
    cnf <- confirmation(a,b,c,d)
    zer <- neutral(a,b,c,d)
    dis <- disconfirmation(a,b,c,d)
    
    Z = NaN*matrix(0, length(a), 1)
    Z[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + c[cnf]) * (c[cnf] + d[cnf]) )
    Z[zer] = 0
    Z[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (a[dis] + c[dis]) * (a[dis] + b[dis]) )
    
    A = NaN*matrix(0, length(a), 1)
    A[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + b[cnf]) * (b[cnf] + d[cnf]) )
    A[zer] = 0;
    A[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (b[dis] + d[dis]) * (c[dis] + d[dis]) )
    sign(A) * A * Z
  },
  "c4" = function(a,b,c,d,alpha=1,beta=1) {
    result <- NaN*matrix(0, length(a), 1)
    cnf <- confirmation(a,b,c,d)
    zer <- neutral(a,b,c,d)
    dis <- disconfirmation(a,b,c,d)
    
    Z = NaN*matrix(0, length(a), 1)
    Z[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + c[cnf]) * (c[cnf] + d[cnf]) )
    Z[zer] = 0
    Z[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (a[dis] + c[dis]) * (a[dis] + b[dis]) )
    
    A = NaN*matrix(0, length(a), 1)
    A[cnf] = (a[cnf]*d[cnf] - b[cnf]*c[cnf]) / ( (a[cnf] + b[cnf]) * (b[cnf] + d[cnf]) )
    A[zer] = 0;
    A[dis] = (a[dis]*d[dis] - b[dis]*c[dis]) / ( (b[dis] + d[dis]) * (c[dis] + d[dis]) )
    
    result[cnf] = apply(cbind(A[cnf],Z[cnf]), 1, min)
    result[zer] = 0;
    result[dis] = apply(cbind(A[dis],Z[dis]), 1, max)
    
    result
  },
  "Phi" = function(a,b,c,d,alpha=1,beta=1){ ( a*d - b*c ) / sqrt( (a+c) * (a+b) * (b+d) * (c+d) ) },
  "Matthews correlation coefficient" = function(a,b,c,d,alpha=1,beta=1){ measureList[["Phi"]](a,b,c,d,alpha,beta)},
  "Determinant of the matrix" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
    
    for (q in 1:length(a)) {
      A = matrix(c(a[q], b[q], c[q], d[q]), nrow=2, ncol=2)
      result[q] = det(A)
    }
    
    result
  },
  "Trace" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
    
    for (q in 1:length(a)) {
      result[q] = a[q] + d[q]
    }
    
    result
  },
  "Absolute value of the determinant of the matrix" = function(a,b,c,d,alpha=1,beta=1) { abs(measureList[["Determinant of the matrix"]](a,b,c,d,alpha,beta)) },
  "First singular value" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
    
    for (q in 1:length(a)) {
      A = matrix(c(a[q], b[q], c[q], d[q]), nrow=2, ncol=2)
      s = svd(A)
      result[q] = s$d[1]
    }
    
    result
  },
  "Second singular value" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
    
    for (q in 1:length(a)) {
      A = matrix(c(a[q], b[q], c[q], d[q]), nrow=2, ncol=2)
      s = svd(A)
      result[q] = s$d[2]
    }
    
    result
  },
  "Second/First singular value" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
  
    for (q in 1:length(a)) {
      A = matrix(c(a[q], c[q], b[q], d[q]), nrow=2, ncol=2)
      s = svd(A)
      result[q] = s$d[2]/s$d[1]
    }
    
    result
  },
  "First * Second singular value" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
    
    for (q in 1:length(a)) {
      A = matrix(c(a[q], b[q], c[q], d[q]), nrow=2, ncol=2)
      s = svd(A)
      result[q] = s$d[1]*s$d[2]
    }
    
    result
  },
  "Real part of the first eigenvalue" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
    
    for (q in 1:length(a)) {
      A = matrix(c(a[q], b[q], c[q], d[q]), nrow=2, ncol=2)
      result[q] = Re(eigen(A)$values[1])
    }
    
    result
  },
  "Real part of the second eigenvalue" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
    
    for (q in 1:length(a)) {
      A = matrix(c(a[q], b[q], c[q], d[q]), nrow=2, ncol=2)
      result[q] = Re(eigen(A)$values[2])
    }
    
    result
  },
  "Minimum of real parts of eigenvalues" = function(a,b,c,d,alpha=1,beta=1) {
    result = matrix(0, length(a), 1)
    
    for (q in 1:length(a)) {
      A = matrix(c(a[q], b[q], c[q], d[q]), nrow=2, ncol=2)
      result[q] = min(Re(eigen(A)$values))
    }
    
    result
  },
  "Chi^2" = function(a,b,c,d,alpha=1,beta=1) {( a*d - b*c )^2 / ( (a+c) * (a+b) * (b+d) * (c+d) )}
)

confirmationDefinition <- list(
  "universal"= function(a,b,c,d,alpha=1,beta=1){ a*d - b*c },
  "bayesian"= function(a,b,c,d,alpha=1,beta=1){a / (a+c) - (a+b) / (a+b+c+d) },
  "strong bayesian"= function(a,b,c,d,alpha=1,beta=1){a / (a+c) - b / (b+d) },
  "likelihood"= function(a,b,c,d,alpha=1,beta=1){ a / (a+b) - (a+c) / (a+b+c+d) },
  "strong likelihood"= function(a,b,c,d,alpha=1,beta=1){ a / (a+b) - c / (c+d) }
)

confirmation <- function(a, b, c, d, mode="universal"){
  confirmationDefinition[[mode]](a, b, c, d) >  0
}

neutral <- function(a, b, c, d, mode="universal"){
  confirmationDefinition[[mode]](a, b, c, d) ==  0
}

disconfirmation <- function(a, b, c, d, mode="universal"){
  confirmationDefinition[[mode]](a, b, c, d) <  0
}

classificationMeasures <- c("True positive rate", "False negative rate", "False positive rate", "True negative rate", "Accuracy", "Balanced accuracy", "Sensitivity", "Precision", "Negative predictive value", "Recall", "Specificity", "G-mean", "F1 score", "Pointwise AUC-ROC", "Kappa", "Matthews correlation coefficient", "Diagnostic odds ratio", "Area Under Lift", "Fb-measure", "Jaccard coefficient", "Optimized precision", "IBA(Accuracy)", "IBA(F1 score)", "IBA(Fb-measure)", "IBA(G-mean)", "Log odds-ratio", "Positive predictive value")

interestingnessMeasures <- c("D", "M", "S", "N", "C", "F", "Z", "A", "c1", "c2", "c3", "c4", "Lift", "Certainty Factor", "Piatetsky-Shapiro")

isClassificationMeasure <- function(measure) {
  measure %in% classificationMeasures
}

measureParameters <- list(
  "IBA(G-mean)" = list("alpha" = list(min = 0, max = 1, default = 0.5, step = 0.05)),
  "IBA(F1 score)" = list("alpha" = list(min = 0.05, max = 5, default = 1, step = 0.05)),
  "IBA(Fb-measure)" = list("alpha" = list(min = 0.05, max = 5, default = 1, step = 0.05),
                           "beta" = list(min = 0.05, max = 5, default = 1, step = 0.05)),
  "IBA(Accuracy)" = list("alpha" = list(min = 0, max = 1, default = 0.25, step = 0.05)),
  "Fb-measure" = list("beta" = list(min = 0.05, max = 5, default = 1, step = 0.05)),
  "c1" = list("alpha" = list(min = 0, max = 1, default = 0.5, step = 0.05)),
  "c2" = list("alpha" = list(min = 0, max = 1, default = 0.5, step = 0.05))
)
