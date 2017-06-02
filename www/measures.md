## Classification measures

| Actual \\ Predicted   | Positive  | Negative  | total  |
|:-:|:-:|:-:|:-:|
| **Positive**  | $\mathit{TP}$  | $\mathit{FN}$ |  $P$ |
| **Negative**  | $\mathit{FP}$  | $\mathit{TN}$ |  $N$ |
| **total**  |  $\widehat{P}$ | $\widehat{N}$  | $n$  |

$$\begin{align}
  \text{Accuracy}& =\frac{\mathit{TP}+\mathit{TN}}{\mathit{TP}+\mathit{TN}+\mathit{FP}+\mathit{FN}}\\\\
  \text{Area Under Lift}& = \frac{1}{2}\frac{P}{n} + \frac{1}{2}(1-\frac{P}{n})\frac{TP}{P}\frac{TN}{N}\\\\
  \text{Balanced accuracy}& = \frac{1}{2}(\frac{\mathit{TP}}{P} + \frac{\mathit{TN}}{N})\\\\
  \text{Diagnostic odds ratio}& = \frac{\mathit{TP} \cdot \mathit{TN}}{\mathit{FN} \cdot \mathit{FP}}\\\\
  \text{F$_1$ score}& = 2 \cdot \frac{\text{recall} \cdot \text{precision}}{\text{precision} + \text{recall} }\\\\
  \text{False negative rate}& = \frac{\mathit{FN}}{\mathit{FN} +\mathit{TP}} = \frac{\mathit{FN}}{P}\\\\
  \text{False positive rate}& = \frac{\mathit{FP}}{\mathit{FP}+\mathit{TN}} = \frac{\mathit{FN}}{N}\\\\
  \text{F$_{\beta}$-measure}& = (1+\beta^2) \cdot \frac{\text{recall} \cdot \text{precision}}{(\beta^2 \cdot \text{precision}) + \text{recall}}\\\\
  \text{G-mean}& = \sqrt{\frac{\mathit{TP}}{\mathit{TP}+\mathit{FN}} \cdot \frac{\mathit{TN}}{\mathit{FP}+\mathit{TN}}} = \sqrt{\text{sensitivity} \cdot \text{specificity}}\\\\
  \text{IBA(Accuracy)}& = (1 + \alpha(\text{sensitivity}-\text{specificity})) \cdot \text{accuracy}\\\\
  \text{IBA(F$_1$ score)}& = (1 + \alpha(\text{sensitivity}-\text{specificity})) \cdot \text{F$_1$ score}\\\\
  \text{IBA(G-mean)}& =   (1 + \alpha(\text{sensitivity}-\text{specificity})) \cdot \text{G-mean}\\\\
  \text{Jaccard coefficient}& = \frac{\mathit{TP}}{\mathit{TP}+\mathit{FP}+\mathit{FN}} \\\\
  \text{Kappa}& = \frac{\text{accuracy} - \frac{1}{n}(\frac{P \cdot \widehat{P}}{n} + \frac{N \cdot \widehat{N}}{n})}{1 - \frac{1}{n}(\frac{P \cdot \widehat{P}}{n} + \frac{N \cdot \widehat{N}}{n}) }\\\\
  \text{Log odds-ratio}& = \log(\text{diagnostic odds ratio})\\\\
  \text{Matthews correlation coefficient}& = \frac{\mathit{TP}\cdot\mathit{TN} - \mathit{FP}\cdot\mathit{FN}}{\sqrt{P \cdot \widehat{P} \cdot N \cdot \widehat{N}}}\\\\
  \text{Negative predictive value}& = \frac{\mathit{TN}}{\mathit{FN}+\mathit{TN}}\\\\
  \text{Optimized precision}& = \text{accuracy} - \frac{|\text{specificity} - \text{sensitivity}|}{\text{specificity} + \text{sensitivity}}\\\\
  \text{Pointwise AUC-ROC}& = \frac{\mathit{TP}}{\mathit{TP}+\mathit{FN}} \cdot \frac{\mathit{TN}}{\mathit{FP}+\mathit{TN}}\\\\
  \text{Positive predictive value}& = \frac{\mathit{TP}}{\mathit{TP}+\mathit{FP}} = \text{precision}\\\\
  \text{Precision}& = \frac{\mathit{TP}}{\mathit{TP}+\mathit{FP}}\\\\
  \text{Recall}& = \frac{\mathit{TP}}{\mathit{TP}+\mathit{FN}} = \text{true positive rate}\\\\
  \text{Sensitivity}& = \frac{\mathit{TP}}{\mathit{TP}+\mathit{FN}} = \text{true positive rate}\\\\
  \text{Specificity}& = \frac{\mathit{TN}}{\mathit{TN}+\mathit{FP}} = \text{true negative rate}\\\\
  \text{True negative rate}& = \frac{\mathit{TN}}{\mathit{TN}+\mathit{FP}}\\\\
  \text{True positive rate}& = \frac{\mathit{TP}}{\mathit{TP}+\mathit{FN}}
\end{align}$$

## Interestingness measures

|    | $H$  | $\neg H$ | $\sum$  |
|:-:|:-:|:-:|:-:|
| **$E$**       | $a$   | $c$    |  $a+c$ |
| **$\neg E$**  | $b$   | $d$    |  $b+d$ |
| **$\sum$**    | $a+b$ | $c+d$  |  $n$  |

$$\begin{align}
  A(H,E)& =
			\begin{cases}
				\dfrac{P(E|H)-P(E)}{1-P(E)}=\dfrac{ad-bc}{(a+b)(b+d)} \quad \text{in case of confirmation}\\\\
				\dfrac{P(H)-P(H| \neg E)}{1-P(H)}=\dfrac{ad-bc}{(b+d)(c+d)} \quad \text{in case of disconfirmation}\\\\
			\end{cases}\\\\
	C(H,E)&=P(E \wedge H)-P(E)P(H)=\dfrac{a}{n}-\dfrac{(a+c)(a+b)}{n^{2}}=\dfrac{ad-bc}{n^2}\\\\
	c_1(H,E)&=
			\begin{cases}        
				\alpha + (1-\alpha) A(H,E) \quad \text{in case of confirmation when } c=0\\\\
				\alpha Z(H,E) \quad \text{in case of confirmation when } c>0 \\\\
				\alpha Z(H,E) \quad \text{in case of disconfirmation when } a>0 \\\\
				-\alpha + (1-\alpha) A(H,E) \quad \text{in case of disconfirmation when } a=0 \\\\
			\end{cases}\\\\
	c_2(H,E)&=
			\begin{cases}
				\alpha + (1-\alpha) Z(H,E) \quad \text{in case of confirmation when } b=0\\\\
				\alpha A(H,E) \quad \text{in case of confirmation when } b>0\\\\
				\alpha A(H,E) \quad \text{in case of disconfirmation when } d>0\\\\
				-\alpha + (1-\alpha) Z(H,E) \quad \text{in case of disconfirmation when } d=0\\\\
			\end{cases}\\\\
	c_3(H,E)&=
			\begin{cases}
				A(H,E)Z(H,E) \quad \text{in case of confirmation}\\\\
				-A(H,E)Z(H,E) \quad \text{in case of disconfirmation}\\\\
			\end{cases}\\\\
	c_4(H,E)&=
			\begin{cases}
				min(A(H,E),Z(H,E)) \quad \text{in case of confirmation}\\\\
				max(A(H,E),Z(H,E)) \quad \text{in case of disconfirmation}\\\\
			\end{cases}\\\\
	\text{Certainty factor}&=\dfrac{P(H|E)-P(H)}{1-P(H)}=\dfrac{\dfrac{a}{a+c}-\dfrac{a+b}{n}}{1-\dfrac{a+b}{n}}\\\\
	D(H,E)&=P(H|E)-P(H)=\dfrac{a}{a+c}-\dfrac{a+b}{n}=\dfrac{ad-bc}{n(a+c)}\\\\
	F(H,E)&=\dfrac{P(E|H)-P(E| \neg H)}{P(E|H)+P(E| \neg H)}=\dfrac{\dfrac{a}{a+b}-\dfrac{c}{c+d}}{\dfrac{a}{a+b}+\dfrac{c}{c+d}}=\dfrac{ad-bc}{ad+bc+2ac}\\\\
	M(H,E)&=P(E|H)-P(E)=\dfrac{a}{a+b}-\dfrac{a+c}{n}=\dfrac{ad-bc}{n(a+b)}\\\\
	N(H,E)&=P(E|H)-P(E|\neg H)=\dfrac{a}{a+b}-\dfrac{c}{c+d}=\dfrac{ad-bc}{(a+b)(c+d)}\\\\
	\text{Piatetsky-Shapiro}&=P(E \wedge H)-P(E)P(H)=\dfrac{a}{n}-\dfrac{(a+c)(a+b)}{n^{2}}=\dfrac{ad-bc}{n^2}\\\\
	S(H,E)&=P(H|E)-P(H|\neg E)=\dfrac{a}{a+c}-\dfrac{b}{b+d}=\dfrac{ad-bc}{(a+c)(b+d)}\\\\
	Z(H,E)&=
			\begin{cases}
				1-\dfrac{P( \neg H|E)}{P(\neg H)}=\dfrac{ad-bc}{(a+c)(c+d)} \quad \text{in case of confirmation}\\\\
				\dfrac{P(H|E)}{P(H)}-1=\dfrac{ad-bc}{(a+c)(a+b)} \quad \text{in case of disconfirmation}\\\\
			\end{cases}
\end{align}$$

<a href="#" onclick="$('body,html').animate({scrollTop : 0}, 500);" class="return-to-top" title="Scroll to top"><i class="fa fa-chevron-up"></i></a>
