# �������� � �������������� ����� is.XYS as.XYZ 

# ��������� �������� 33.3 ���������� my_numeric
my_numeric <- 33.3
  
# ���������� �������� my_character � "33"
my_character <- "������"
my_character <- "33"

# ��������� ��� ������ my_character
is.numeric(my_character)

# ������������ my_character � ����� ��� ������ ������� as.numeric � �������� � ���������� my_numeric_character
my_numeric_character<-as.numeric(my_character)

# ����������� ����� my_numeric � my_numeric_character � �������� �� �����
my_numeric+my_numeric_character
