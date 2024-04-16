setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

cat("\014")  

library(DiagrammeR)

mapa <- create_graph()

mapa <- add_node(mapa, label = "Problema",
                  type = "problema",
                  node_aes = node_aes(shape = "rectangle",
                                      height = 0.9,
                                      width = 0.9,
                                      fontsize = 8,
                                      fillcolor = c("white"),
                                      color = c("red")))
mapa <- add_node(mapa, label = "Pergunta", 
                  type = "pergunta",
                  node_aes = node_aes(shape = "triangle",
                                      height = 0.7,
                                      width = 0.7,
                                      fontsize = 5,
                                      fillcolor = c("white"),
                                      color = c("blue")))
mapa <- add_node(mapa, label = "Premissa 1", 
                  type = "premissa",
                  node_aes = node_aes(shape = "plaintext",
                                      fontsize = 5,
                                      color = c("white"),
                                      fillcolor = c("white")))
mapa <- add_node(mapa, label = "Premissa 2", 
                  type = "premissa",
                  node_aes = node_aes(shape = "plaintext",
                                      fontsize = 5,
                                      color = c("white"),
                                      fillcolor = c("white")))
mapa <- add_node(mapa, label = "Premissa 3", 
                  type = "premissa",
                  node_aes = node_aes(shape = "plaintext",
                                      fontsize = 5,
                                      color = c("white"),
                                      fillcolor = c("white")))
mapa <- add_node(mapa, label = "Premissa 4", 
                  type = "premissa",
                  node_aes = node_aes(shape = "plaintext",
                                      fontsize = 5,
                                      color = c("white"),
                                      fillcolor = c("white")))
mapa <- add_node(mapa, label = "Hipótese 1",
                  type = "hipotese",
                  node_aes = node_aes(shape = "ellipse",
                                      fontsize = 5,
                                      fillcolor = c("white"),
                                      color = c("darkgreen")))
mapa <- add_node(mapa, label = "Hipótese 2", 
                  type = "hipotese",
                  node_aes = node_aes(shape = "ellipse",
                                      fontsize = 5,
                                      fillcolor = c("white"),
                                      color = c("darkgreen")))
mapa <- add_node(mapa, label = "Previsão 1",
                  type = "previsao",
                  node_aes = node_aes(shape = "polygon",
                                      sides = 5,
                                      fontsize = 5,
                                      color = c("yellow4"),
                                      fillcolor = c("white")))
mapa <- add_node(mapa, label = "Previsão 2",
                  type = "previsao",
                  node_aes = node_aes(shape = "polygon",
                                      sides = 5,
                                      fontsize = 5,
                                      color = c("yellow4"),
                                      fillcolor = c("white")))


mapa <- add_edge(mapa, "Problema", "Pergunta", edge_aes = NULL, edge_data = NULL)
mapa <- add_edge(mapa, "Pergunta", "Hipótese 1", edge_aes = NULL, edge_data = NULL)
mapa <- add_edge(mapa, "Pergunta", "Hipótese 2", edge_aes = NULL, edge_data = NULL)
mapa <- add_edge(mapa, "Premissa 1", "Hipótese 1", edge_aes = NULL, edge_data = NULL)
mapa <- add_edge(mapa, "Premissa 2", "Hipótese 1", edge_aes = NULL, edge_data = NULL)
mapa <- add_edge(mapa, "Premissa 3", "Hipótese 2", edge_aes = NULL, edge_data = NULL)
mapa <- add_edge(mapa, "Premissa 4", "Hipótese 2", edge_aes = NULL, edge_data = NULL)
mapa <- add_edge(mapa, "Hipótese 1", "Previsão 1", edge_aes = NULL, edge_data = NULL)
mapa <- add_edge(mapa, "Hipótese 2", "Previsão 2", edge_aes = NULL, edge_data = NULL)


mapa %>% get_node_df()

mapa %>% get_edge_df()


render_graph(mapa)

export_graph(mapa, file_name = "mapa_mental.png",
             file_type = "png",
             width = 3000,
             height = 3000)
