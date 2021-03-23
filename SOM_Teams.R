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
som32 = som(datossom, grid = somgrid(3,2,"hexagonal"), rlen=1000)
som33 = som(datossom, grid = somgrid(3,3,"hexagonal"), rlen=1000)
som43 = som(datossom, grid = somgrid(4,3,"hexagonal"), rlen=1000)
som44 = som(datossom, grid = somgrid(4,4,"hexagonal"), rlen=1000)

par(mfrow = c(2,2))
plot(som32, main = "changes 3x2", type = "change", shape = "straight")
plot(som33, main = "changes 3x3", type = "change", shape = "straight")
plot(som43, main = "changes 4x3", type = "change", shape = "straight")
plot(som44, main = "changes 4x4", type = "change", shape = "straight")

par(mfrow = c(2,2))
plot(som32, main = "counts 3x2", type = "counts", shape = "straight")
plot(som33, main = "counts 3x3", type = "counts", shape = "straight")
plot(som43, main = "counts 4x3", type = "counts", shape = "straight")
plot(som44, main = "counts 4x4", type = "counts", shape = "straight")

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

# El mejor es 3x3
par(mfrow = c(1,1))
plot(som33, main = "SOM", palette.name = rainbow, shape = "straight")

par(mfrow = c(2,2))
plot(som33, main = "SOM", palette.name = rainbow, shape = "straight")
plot(som33, type = "counts", shape = "straight")
plot(som33, type = "quality", shape = "straight")
plot(som33, type = "dist.neighbours", shape = "straight")

plot(som33, type = "property", property = getCodes(som33,1)[,1], main = colnames(getCodes(som33,1))[1], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,2], main = colnames(getCodes(som33,1))[2], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,3], main = colnames(getCodes(som33,1))[3], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,4], main = colnames(getCodes(som33,1))[4], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,5], main = colnames(getCodes(som33,1))[5], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,6], main = colnames(getCodes(som33,1))[6], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,7], main = colnames(getCodes(som33,1))[7], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,8], main = colnames(getCodes(som33,1))[8], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,9], main = colnames(getCodes(som33,1))[9], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,10], main = colnames(getCodes(som33,1))[10], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,11], main = colnames(getCodes(som33,1))[11], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,12], main = colnames(getCodes(som33,1))[12], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,13], main = colnames(getCodes(som33,1))[13], palette.name = coolBlueHotRed, shape = "straight")
plot(som33, type = "property", property = getCodes(som33,1)[,14], main = colnames(getCodes(som33,1))[14], palette.name = coolBlueHotRed, shape = "straight")

# SOM 2
som32_2 = som(datossom2, grid = somgrid(3,2,"hexagonal"), rlen=1000)
som33_2 = som(datossom2, grid = somgrid(3,3,"hexagonal"), rlen=1000)
som43_2 = som(datossom2, grid = somgrid(4,3,"hexagonal"), rlen=1000)
som44_2 = som(datossom2, grid = somgrid(4,4,"hexagonal"), rlen=1000)

par(mfrow = c(2,2))
plot(som32_2, main = "changes 3x2", type = "change", shape = "straight")
plot(som33_2, main = "changes 3x3", type = "change", shape = "straight")
plot(som43_2, main = "changes 4x3", type = "change", shape = "straight")
plot(som44_2, main = "changes 4x4", type = "change", shape = "straight")

par(mfrow = c(2,2))
plot(som32_2, main = "counts 3x2", type = "counts", shape = "straight")
plot(som33_2, main = "counts 3x3", type = "counts", shape = "straight")
plot(som43_2, main = "counts 4x3", type = "counts", shape = "straight")
plot(som44_2, main = "counts 4x4", type = "counts", shape = "straight")

# El mejor es 3x2
par(mfrow = c(1,1))
plot(som32_2, main = "SOM", palette.name = rainbow, shape = "straight")

par(mfrow = c(2,2))
plot(som32_2, main = "SOM", palette.name = rainbow, shape = "straight")
plot(som32_2, type = "counts", shape = "straight")
plot(som32_2, type = "quality", shape = "straight")
plot(som32_2, type = "dist.neighbours", shape = "straight")

par(mfrow = c(1,3))
plot(som32_2, type = "property", property = getCodes(som32_2,1)[,1], main = colnames(getCodes(som32_2,1))[1], palette.name = coolBlueHotRed, shape = "straight")
plot(som32_2, type = "property", property = getCodes(som32_2,1)[,2], main = colnames(getCodes(som32_2,1))[2], palette.name = coolBlueHotRed, shape = "straight")
plot(som32_2, type = "property", property = getCodes(som32_2,1)[,3], main = colnames(getCodes(som32_2,1))[3], palette.name = coolBlueHotRed, shape = "straight")

# Vectores de pesos SOM
tabla3x3 = som33$codes
tabla3x2 = som32_2$codes

# Guardar datos
write.csv(tabla3x3, "tabla3x3.csv")
write.csv(tabla3x2, "tabla3x2.csv")

# Asignación nodo a jugadora
nodosequipos1 = data.frame(som33$unit.classif)
nodosequipos2 = data.frame(som32_2$unit.classif)
write.csv(nodosequipos1, "nodosequipos1.csv")
write.csv(nodosequipos2, "nodosequipos2.csv")
