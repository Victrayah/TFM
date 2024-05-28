import pandas as pd
import matplotlib.pyplot as plt

# Cargar el archivo CSV generado desde R
data_plot = pd.read_csv(r'D:\BDSLab_2023\2023_ICU_dellirium_sars_cov_2\data_plot.csv')

# Filtrar datos por tipo de barra
mean_data = data_plot[data_plot['Bar_Type'] == 'Media']
upper_limit_data = data_plot[data_plot['Bar_Type'] == 'Límite superior']
lower_limit_data = data_plot[data_plot['Bar_Type'] == 'Límite inferior']

# Ordenar datos por valor de perplexity
mean_data = mean_data.sort_values('Perplexity_Value')
upper_limit_data = upper_limit_data.sort_values('Perplexity_Value')
lower_limit_data = lower_limit_data.sort_values('Perplexity_Value')

# Configurar valores y etiquetas
perplexity_values = mean_data['Perplexity_Value']
mean_values = mean_data['Value']
upper_values = upper_limit_data['Value']
lower_values = lower_limit_data['Value']

# Crear el gráfico de barras
plt.figure(figsize=(10, 6))

plt.bar(perplexity_values, mean_values, color='skyblue', label='Media')
plt.bar(perplexity_values, upper_values, color='orange', label='Límite Superior', alpha=0.5)
plt.bar(perplexity_values, lower_values, color='green', label='Límite Inferior', alpha=0.5)

# Etiquetas y título
plt.xlabel('Perplexity Value')
plt.ylabel('Valor de mean_overlap')
plt.title('Valores de mean_overlap con Límites para cada perplexity value')
plt.legend()
plt.grid(True)
plt.tight_layout()

# Mostrar el gráfico
plt.show()

