import streamlit as st
import pandas as pd
import plotly_express as px
from PIL import Image



@st.cache
def load_dataset():
	data = pd.read_csv("data_clean.csv", sep=';')
	return data
data = load_dataset()

# Allow users to choose ppart according list below
app_mode = st.sidebar.selectbox("Presentation", ["Presentation", "Statistics",
	"Machine Learning", "Results"])

if app_mode == "Presentation":
	st.title('Web App Project')
	image = Image.open('Image.jpeg')
	st.image(image, use_column_width=True)

	"""
	## Aim of the project
	This aims of this little project is to predcit if a candidates will be 
	employed or not. 
	Here we use the dataset cleaned. According the rules 
	"""


	# Display dataset asking for user 
	if st.sidebar.checkbox("Show dataset"):
		st.dataframe(data)
		'La taille du dataset est', data.shape
	if st.sidebar.checkbox("Show dataset dimension"):
		'La taille du dataset est:', data.shape






if app_mode == "Statistics":
	if st.sidebar.checkbox("Show corrlation matrix"):
		image = Image.open('Cor_mat.png')
		st.image(image, use_column_width=True)


	diplom = st.sidebar.multiselect('Show diplom for candidates ?', data['diplome'].unique())
	st.title("Titre 1 ")
	fig = px.violin(data, x='embauche', y='exp')
	st.plotly_chart(fig)
	fig2 = px.scatter(data, x='age', y='exp')
	st.plotly_chart(fig2)


if app_mode == "Machine Learning":
	"""
	# Algorithme
	Ici j'ai choisis d'utiliser un Random Forest
	"""
	with st.echo():
		for i in range(10):
			print(i)

if app_mode == "Results":
	image = Image.open('Roc_Curve.png')
	st.image(image, use_column_width=True)