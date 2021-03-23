library(kohonen)

# Cargar datos
datos <- read.csv(file.choose(), header = T)

# Goles/Fallos
datos1 = datos[,1:14]

# Roja/2min/Bloqueos
datos2 = datos[,c(15,16,20)]

# Normalization
# norm_data<- scale(data)
# Mejores resultados sin normalizar

datossom = as.matrix(datos1)
datossom2 = as.matrix(datos2)

set.seed(100)

# SOM 1
som76 = som(datossom, grid = somgrid(7,6,"hexagonal"), rlen=1000)
som77 = som(datossom, grid = somgrid(7,7,"hexagonal"), rlen=1000)
som88 = som(datossom, grid = somgrid(8,8,"hexagonal"), rlen=1000)
som98 = som(datossom, grid = somgrid(9,8,"hexagonal"), rlen=1000)

par(mfrow = c(2,2))
plot(som76, main = "changes 7x6", type = "change", shape = "straight")
plot(som77, main = "changes 7x7", type = "change", shape = "straight")
plot(som88, main = "changes 8x8", type = "change", shape = "straight")
plot(som98, main = "changes 9x8", type = "change", shape = "straight")

par(mfrow = c(2,2))
plot(som76, main = "counts 7x6", type = "counts", shape = "straight")
plot(som77, main = "counts 7x7", type = "counts", shape = "straight")
plot(som88, main = "counts 8x8", type = "counts", shape = "straight")
plot(som98, main = "counts 9x8", type = "counts", shape = "straight")

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

# El mejor es 7x7
par(mfrow = c(1,1))
plot(som77, main = "SOM", palette.name = rainbow, shape = "straight")

par(mfrow = c(2,2))
plot(som77, main = "SOM", palette.name = rainbow, shape = "straight")
plot(som77, type = "counts", shape = "straight")
plot(som77, type = "quality", shape = "straight")
plot(som77, type = "dist.neighbours", shape = "straight")

plot(som77, type = "property", property = getCodes(som77,1)[,1], main = colnames(getCodes(som77,1))[1], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,2], main = colnames(getCodes(som77,1))[2], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,3], main = colnames(getCodes(som77,1))[3], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,4], main = colnames(getCodes(som77,1))[4], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,5], main = colnames(getCodes(som77,1))[5], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,6], main = colnames(getCodes(som77,1))[6], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,7], main = colnames(getCodes(som77,1))[7], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,8], main = colnames(getCodes(som77,1))[8], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,9], main = colnames(getCodes(som77,1))[9], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,10], main = colnames(getCodes(som77,1))[10], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,11], main = colnames(getCodes(som77,1))[11], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,12], main = colnames(getCodes(som77,1))[12], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,13], main = colnames(getCodes(som77,1))[13], palette.name = coolBlueHotRed, shape = "straight")
plot(som77, type = "property", property = getCodes(som77,1)[,14], main = colnames(getCodes(som77,1))[14], palette.name = coolBlueHotRed, shape = "straight")

# SOM 2
som33_2 = som(datossom2, grid = somgrid(3,3,"hexagonal"), rlen=1000)
som43_2 = som(datossom2, grid = somgrid(4,3,"hexagonal"), rlen=1000)
som44_2 = som(datossom2, grid = somgrid(4,4,"hexagonal"), rlen=1000)
som54_2 = som(datossom2, grid = somgrid(5,4,"hexagonal"), rlen=1000)

par(mfrow = c(2,2))
plot(som33_2, main = "changes 3x3", type = "change", shape = "straight")
plot(som43_2, main = "changes 4x3", type = "change", shape = "straight")
plot(som44_2, main = "changes 4x4", type = "change", shape = "straight")
plot(som54_2, main = "changes 5x4", type = "change", shape = "straight")

par(mfrow = c(2,2))
plot(som33_2, main = "counts 3x3", type = "counts", shape = "straight")
plot(som43_2, main = "counts 4x3", type = "counts", shape = "straight")
plot(som44_2, main = "counts 4x4", type = "counts", shape = "straight")
plot(som54_2, main = "counts 5x4", type = "counts", shape = "straight")

# El mejor es 4x4
par(mfrow = c(1,1))
plot(som44_2, main = "SOM", palette.name = rainbow, shape = "straight")

par(mfrow = c(2,2))
plot(som44_2, main = "SOM", palette.name = rainbow, shape = "straight")
plot(som44_2, type = "counts", shape = "straight")
plot(som44_2, type = "quality", shape = "straight")
plot(som44_2, type = "dist.neighbours", shape = "straight")

par(mfrow = c(1,3))
plot(som44_2, type = "property", property = getCodes(som44_2,1)[,1], main = colnames(getCodes(som44_2,1))[1], palette.name = coolBlueHotRed, shape = "straight")
plot(som44_2, type = "property", property = getCodes(som44_2,1)[,2], main = colnames(getCodes(som44_2,1))[2], palette.name = coolBlueHotRed, shape = "straight")
plot(som44_2, type = "property", property = getCodes(som44_2,1)[,3], main = colnames(getCodes(som44_2,1))[3], palette.name = coolBlueHotRed, shape = "straight")

# Vectores de pesos SOM
tabla7x7 = som77$codes
tabla4x4 = som44_2$codes

# Guardar datos
write.csv(tabla7x7, "tabla7x7.csv")
write.csv(tabla4x4, "tabla4x4.csv")

# Asignación nodo a jugadora
nodosjugadoras1 = data.frame(som77$unit.classif)
nodosjugadoras2 = data.frame(som44_2$unit.classif)
write.csv(nodosjugadoras1, "nodosjugadoras1.csv")
write.csv(nodosjugadoras2, "nodosjugadoras2.csv")