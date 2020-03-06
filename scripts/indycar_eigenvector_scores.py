import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

data = pd.read_csv("C:/users/drewb/Desktop/am.csv")
data = data.set_index('name')
print(data)

columns = list(data)

G = nx.DiGraph()

#for each row
for index, row in data.iterrows():
	G.add_node(index)
	#for each column
	for i in columns:
		print(index, i, data[index][i])
		#add edge
		G.add_edge(i, index, weight=data[index][i])


A = nx.adjacency_matrix(G).todense()

centrality = nx.eigenvector_centrality(G, weight = "weight", max_iter = 5000)
#centrality = nx.katz_centrality(G, weight = "weight", max_iter = 5000, tol = .05, alpha = 1, beta = 0)
print(['%s %0.2f'%(node,centrality[node]) for node in centrality])

#Output results to df
x = pd.DataFrame({'driv':[], 'score':[]})
for i in centrality:
	print(i, centrality[i])
	temp=pd.DataFrame({'driv':[i], 'score':[centrality[i]]})
	x = x.append(temp)

x.to_csv("theanswer.csv")

'''
nx.draw_circular(G, with_labels=True, node_size=300, alpha=.1)
plt.title('eigenvector centrality')
plt.show()
'''


