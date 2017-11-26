#��������� ������ � ���������. �����: github    https://raw???����_�_�����_�������_����???/data/gmp.dat 
gmp <- read.table(file="https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/gmp.dat")
gmp$pop <- gmp$gmp/gmp$pcgmp

estimate.scaling.exponent <- function(a, y0=6611, response=gmp$pcgmp,
                                      predictor = gmp$pop, maximum.iterations=100, deriv.step = 1/100,
                                      step.scale = 1e-12, stopping.deriv = 1/100) {
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (abs(deriv) <= stopping.deriv) { break() }
  }
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  return(fit)
}
#������ ������ � ��������� ��������� a
a.before.deletion<-estimate.scaling.exponent(0.15);
a.before.deletion

#� ������� ����������� ������������ ��������� ������ (������� curve) �����������
curve((y0=6611)*x^estimate.scaling.exponent(0.15)$a,gmp$pop)

#������� ����� �� ������ �������� ������ ��������� �������, ��� ���������� �������������� ������ ������������ a?
gmp<-gmp[-sample(x = 1:366, size = 1),]
a.after.deletion<-estimate.scaling.exponent(0.15)
a.after.deletion
a.after.deletion$a-a.before.deletion$a
#�������� a ����������� �� 2.827777e-05

#��������� ������ ��������� ��� � ������ ��������� �����. ��� ���������� �������� a?
q<-estimate.scaling.exponent(0) #a=0.1212198, 77 ��������, ��������
estimate.scaling.exponent(0.1) #a=0.1212198, 61 ��������, ��������
estimate.scaling.exponent(0.2198) #a=0.1212198, 99 ��������, ��������
estimate.scaling.exponent(0.2199) #a=0.1209941, 100 ��������, ����������
estimate.scaling.exponent(0.3) #a=-2.861204, 2 ��������, ��������
estimate.scaling.exponent(3) #a=-2.394772e+38, 2 ��������, ��������
estimate.scaling.exponent(-0.124); #a=0.1212197, 100 ��������, ����������
estimate.scaling.exponent(-0.123); #a=0.1212198, 100 ��������, ����������
estimate.scaling.exponent(-3); #a=-3, 1 ��������, ��������
#� ��������� ������ ������������� �������� 0.1212198 ��� ��������� ��������� ����� � ���������� [-0.124, 0.2198].
