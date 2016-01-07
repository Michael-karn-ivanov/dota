from dao.dao import Dao
from providers.coeffprovider import CoeffProvider

__author__ = 'mivanov'

dao = Dao()
coeffProvider = CoeffProvider()
coeffs = coeffProvider.get_coeff_list()
dao.save_coeff_list(coeffs)