from sklearn.preprocessing import OrdinalEncoder


def Coding(data,scala, var_name):
  for i in data.columns:
    encoder = OrdinalEncoder(categories=[scala])
    encoder.fit((data[[i]]))
    data[var_name+i]=encoder.transform(data[[i]])