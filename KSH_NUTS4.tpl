<!--head
Title: Kistérségi adatok térképezése és elemzése
Description: A KSH településszintű adatain alapulva kistérségi szintű térképező és elemző
Author: Gergely Tóth (@tothg)
Packages: classInt, ineq, maptools, RcolorBrewer, sp, spdep
Data required: TRUE

plotvars | *numeric[1,8] | Elemezni kívánt változó | Itt sorolhatja fel azon változókat, amelyek elemzését végre kívánja hajtani. Több változót is megadhat (maximum 8-at egyszerre), amely esetben összehasonlító elemzést is kapunk az elemzés végén.
normvar | numeric[1,1] | Normalizáló változó | A célváltozót normalizálja vissza, és ilyen módon arányt képez (pl.: népesség számához viszonyítás stb.) - Opcionális és csak egy változót adhat meg.
nclr | 5,3,4,6,7,8,9 | Színátmenet kategóriának száma | Azt lehet kiválasztani, hogy a térképezés során hány különböző, a színezéshes szükséges kategóriát hozzunk létre.
szinskala | Blues,BuGn,BuPu,GnBu,Greens,Greys,Oranges,OrRd,PuBu,PuBuGn,PuRd,Purples,RdPu,Reds,YlGn,YlGnBu,YlOrBr,YlOrRd | Alkalmazni kívánt színskála | A térképezés során alkalmazni kívánt színskála.
reverse | FALSE | Fordított színskála | Kiválasztása esetén a térképek színátmenetének sorrendje fordított lesz.
onlymap | FALSE | Csak térképezés | Kiválasztása esetén az elemzés során csak térképek készülnek szöveges elemzés nélkül.
styleb | quantile,sd,pretty,kmeans,hclust,bclust,fisher,jenks,equal | Színskála töréspontjainak meghatározása | Azt a statisztikai eljárást lehet kiválasztani, amely  meghatározza a töréspontokat a színskála leképezésekor.
namevar | numeric[1,1] | Kistérség azonosítója (opcionális): | Saját adatbázis esetén a kistérség azonosítóját tartalmazó változó! Megadása csak akkor kötelező, ha eltér a "NUTS4CODE" névtől!
head-->

<%=
evalsOptions('graph.unify', FALSE)
panderOptions('p.copula', ' és ')
data('HUN-maps')
nclr <- as.numeric(nclr)
plotclr <- brewer.pal(nclr, szinskala)
missingclr <- "#333333"
if (szinskala == "Greys") {
missingclr <- "yellow"
}
if (reverse) {
	plotclr <- plotclr[nclr:1]
}
emlekezteto <- 1
result_list <- list()
szamlalo <- 1
%>

<%=
if (length(namevar)==0) {
	namevar <- rp.data$NUTS4CODE
}
%>

<%
if (!all(HUN_kisterseg@data$NUTS4CODE %in% namevar))
{ %> A megadott kistérség azonosító nem érvényes. Kérjük, olyan csatolóváltozót használjon,
amelyben a következő érvényes NUTS-4 kódok szerepelnek:
<%= kiir <- data.frame(NUTS4CODE = as.character(HUN_kisterseg@data$NUTS4CODE),
	kistérségnév = as.character(HUN_kisterseg@data$NUTS4NAME),
	stringsAsFactors = FALSE)
kiir
%>
<% } else { %>

<%=
mapping <- function() {
	plot(HUN_kisterseg, col=colcode)
	title(main="", sub=gsub("_","", cimke, fixed=TRUE))
	if (is.null(nas)) {
		if (is.null(szazalek)) {
			legend("bottomright",
				legend=gsub("[", "",
					gsub("]", "",
					gsub(",", "-",
					gsub( ")", "",
					names(attr(colcode, "table")),
					fixed=TRUE), fixed=TRUE),
					fixed=TRUE), fixed=TRUE),
					fill=attr(colcode, "palette"))
		} else { 
			legend("bottomright",
				legend=gsub("[", "",
					gsub("]", "%",
					gsub(",", "%-",
					gsub(")", "%",
					names(attr(colcode, "table")),
					fixed=TRUE), fixed=TRUE),
					fixed=TRUE), fixed=TRUE),
					fill=attr(colcode, "palette"))
		}
	} else {
		if (is.null(szazalek)) {
			legend("bottomright",
			legend=c("NA/Inf",
				gsub("[", ""
				gsub("]", "",
				gsub(",", "-",
				gsub( ")", "",
				names(attr(colcode, "table")),
				fixed=TRUE), fixed=TRUE),
				fixed=TRUE), fixed=TRUE)),
				fill=c(missingclr, attr(colcode, "palette")))
		} else { 
			legend("bottomright",
			legend=c("NA/Inf",
				gsub("[", "",
				gsub("]", "%",
				gsub(",", "%-",
				gsub(")", "%",
				names(attr(colcode, "table")),
				fixed=TRUE), fixed=TRUE),
				fixed=TRUE), fixed=TRUE)),
				fill=c(missingclr, attr(colcode, "palette")))
		}
	}
}
mapping2 <- function() {
    plot(HUN_kisterseg, col = colcode)
    title(main = "",
	sub = gsub("_", "", cimke, fixed = TRUE))
    legend("bottomright",
	legend = attr(colcode, "table"),
	fill = attr(colcode, "palette")) 
}
    
lcp <- function() {
	plot(Lc(na.omit(plotvar.2)), main = "", sub = "")
	title(main = "Lorenz görbe")
	abline(0, 1, col = "red", lwd = 2)
}

ineq_s <- function (x,
	parameter = NULL,
	type = c("Gini", "RS", "Atkinson", "Theil", "Kolm", "var", "square.var", "entropy")) {
		parameter <<- "no"
		if (type == "Atkinson") { parameter <<- 0.5 }
		if (type == "Theil") { parameter <<- 0 } 
		if (type == "Kolm") { parameter <<- 1 }
		if (type == "entropy") { parameter <<- 0.5 }	
		switch(match.arg(type),
			Gini = Gini(x),
			RS = RS(x),
			Atkinson = Atkinson(x, parameter = 0.5),
			Theil = Theil(x, parameter = 0),
			Kolm = Kolm(x, parameter = 1),
			var = var.coeff(x),
			square.var = var.coeff(x, square = TRUE),
			entropy = entropy(x, parameter = 0.5))
}
%>

<%= if (is.null(dim(plotvars))) {
plotvars <- as.data.frame(plotvars) } %>

<%= kist.nb <- poly2nb(HUN_kisterseg)
col.W <- nb2listw(kist.nb, style="W") %>

<% for (ix in 1:length(plotvars)) { %>

<%=
result_list[[szamlalo]] <- list()
plotvar <- plotvars[,ix]
cimke <- rp.label(plotvar)
if (!is.null(normvar)) {
	cimke <- paste0(cimke, " _normalizálva -_ ", rp.label(normvar), " _változóval_")
} %>
<%= 
felirat <- rp.label(plotvar)
if (!is.null(normvar)) {
	felirat <- paste0(felirat, " -normalizált")
} 

cimke <- rp.label(plotvar)
if (!is.null(normvar)) {
	cimke <- paste0(strwrap(paste(cimke,
		"_normalizálva_ ",
		rp.label(normvar),
		" _változóval._"), 80), collapse="\n")
} 
result_list[[szamlalo]][['név']] <- gsub("_",
	"", gsub("\n", " ",
	cimke,
	fixed = TRUE), fixed = TRUE)
%>

<%=
if (!is.null(normvar)) {
	plotvar <- plotvar/normvar
}

result_list[[szamlalo]][['plotvar']] <- plotvar  
### A célváltozó eloszlásának vizsgálata

if (length(plotvars)>1) {
set.caption(paste0('Leíró statisztikák -', as.roman(ix), '.')) } else {
set.caption('Leíró statisztikák') }

summary(plotvar) %>
<%if (any(is.na(plotvar)) | any(plotvar == Inf)) { %>
<%= 
invalid <- NULL
invalid <- c(invalid, namevar[which(is.na(plotvar))])
invalid <- c(invalid, namevar[which(plotvar != Inf)])

paste0('A célváltozó a következő kistérségek esetén érvénytelen-', rp.label(namevar), ':') 
p(invalid)

plotvar.valid <- plotvar[which(!is.na(plotvar))]
plotvar.valid <- plotvar.valid[which(plotvar.valid != Inf)]
set.caption('A célváltozó érvényes értékeinek az eloszlása') 
summary(plotvar.valid)
result_list[[szamlalo]][['plotvar.valid']] <- plotvar.valid  
%>
<% } %>

### A célváltozó területi eloszlásának vizsgálata
<%=

plotvar.2 <- plotvar[match( HUN_kisterseg@data$NUTS4CODE, namevar)]
namevar.2 <- namevar[match( HUN_kisterseg@data$NUTS4CODE, namevar)]

totvar <- data.frame(Kistérség=HUN_kisterseg@data$NUTS4NAME, NUTS4CODE = namevar.2, Érték = plotvar.2)
totvar <- totvar[sort(totvar$Érték, decreasing = TRUE, index.return = T)$ix, ]
rownames(totvar) <- 1:length(namevar.2)
result_list[[szamlalo]][['totvar']] <- totvar  
%>

<% if (length(unique(plotvar)) > 2) { %>
<%=
szazalek <- NULL
nas <- NULL
if (max(plotvar.2) < 1) {
	plotvar.2 <- plotvar.2*100
	szazalek <- 1
}

plotvar.2[which(plotvar.2==Inf)] <- NA

class <- suppressWarnings(classIntervals(plotvar.2, nclr, style = styleb))
class$brks <- as.numeric(sprintf("%.2f", class$brks))

colcode <- suppressWarnings(findColours(class, plotclr))
if (length(which(is.na(plotvar.2))) >0) {
	colcode[which(is.na(plotvar.2))] <- missingclr
	nas <- 1
}

if (length(plotvars) > 1) {
	set.caption(paste0('Kistérségi szintű térképi ábra - ', as.roman(ix), '. változó'))
} else {
	set.caption('Kistérségi szintű térképi ábra')
}
mapping()
%>
<% if (!onlymap) { %> 

<%= set.caption('Legmagasabb értékű kistérségek') 
ira <- totvar[(!is.na(totvar$Érték)),]
ira <- ira[ira$Érték != Inf,]
ira <- ira[(1:6)[(1:6) <= nrow(ira)],]
ira$Érték <- round(ira$Érték, 3) 
rownames(ira) <- 1:nrow(ira) 
ira
result_list[[szamlalo]][['maxi']] <- ira
maxi <- ira[1, ] %>
<%= set.caption('Legalacsonyabb értékű kistérségek')
ir <- totvar[(!is.na(totvar$Érték)), ]
ir <- ir[ir$Érték != Inf, ]
ir <- ir[-(1:(nrow(ir) - 6))[(1:(nrow(ir) - 6)) > 0], ]
ir$Érték <- round(ir$Érték, 3) 
rownames(ir) <- 1:nrow(ir)
ir 
result_list[[szamlalo]][['mini']] <- ir
mini <- tail(ir, 1)%>

* Tehát a legmagasabb értékkel bíró <%= nrow(ira) %> kistérség: <%= as.character(ira$Kistérség) %>,
míg a legalacsonyabb értékkel bíró <%= nrow(ir) %> kistérség: <%= as.character(ir$Kistérség)%>.
Az adatok alapján elmondhatjuk, hogy <%= maxi$Kistérség %> kistérség értékei a <%= mini$Kistérség %>
kistérség értékeit <%=rata =maxi$Érték/mini$Érték; if (rata>2) {
		'sokszorosan'
	} else {
		'nem túl nagy mértékben'
	}  %> múlják felül. <% if (rata!=Inf) { %> A két szélső érték egymáshoz viszonyított aránya <%= rata  %>. <% } %>
### A célváltozó eloszlásának vizsgálata egyenlőtlenségi szempontból

<%= 
plotvar.2b <- na.omit(plotvar.2) 
gni <- round(ineq_s(plotvar.2b, type = "Gini"),3) 
if (gni<0.3) {
	beir <- ' nem mutat túlzott területi egyenlőtlenséget'
} else {
	if (gni<0.7) {
		beir <- ' közepesen erős területi egyenlőtlenséget mutat'
	} else {
		beir <- ' szélsőségesen erős egyenlőtlenséget mutat'
	}
}%>
<%= paste0("* Az elemzett változóra számítótt Gini-index (", gni, ")", beir, ".") %> 
<% if (emlekezteto == 1) { %>
_Emlékeztetőül: A [Gini-index](http://en.wikipedia.org/wiki/Gini_coefficient) egy olyan mérőszám, melyet legfőképpen a közgazdaságtanban alkalmaznak a statisztikai eloszlások egyenlőtlenségeinek mérésére (így például jövedelemi egyenlőtlenségek vizsgálatára). 
A Gini-index értéke 0-1 értéket veheti fel, ahol az elméleti 0 érték fejezi ki a tökéletesen egyenletes eloszlást, míg minél inkább közelít az egyhez, annál egyenlőtlenebb az eloszlás._
<% } %>
<%= evalsOptions('graph.unify', TRUE) %>
<%= 
result_list[[szamlalo]][['plotvar.2']] <- plotvar.2
set.caption(felirat)
lcp() %>
<%= evalsOptions('graph.unify', FALSE) 
atkinson <- ineq_s(plotvar.2b, type = "Atkinson")
theil <- ineq_s(plotvar.2b, type = "Theil")
entrop <- ineq_s(plotvar.2b, type = "entropy")
kolm <- ineq_s(plotvar.2b, type = "Kolm")
%>

<% if (emlekezteto == 1) { %>
_Emlékeztetőül: Az egyenlőtlenséget vizsgálni szokták grafikus módon is, az úgynevezett [Lorenz-görbe](http://en.wikipedia.org/wiki/Lorenz_curve) segítségével, amelyet Max Otto Lorenz fejlesztett ki a 20. század elején. A Lorenz-görbe esetén az eloszlás egyenlőtlenségét egy négyzet alakú koordinátarendszerben felrajzolt görbe segítségével elemezhetjük: minél inkább közelít a görbe az átlóhoz (hasonlóan, mint Gini-index értéke a 0-hoz), annál inkább egyenletes  az eloszlás, míg a görbének az átlótól való távolodása az egyenlőtlenség fokmérője._

* Ugyanakkor a Gini-index mellett más egyenlőtlenségi mutatók is ismertek, így többek között az Atkinson féle mutató (amelynek értéke=<%= atkinson%> - <%=parameter%> paraméter esetén), a Theil-index (amelynek értéke=<%= theil %> - <%=parameter%> paraméter esetén), az Entrópia-index (amelynek értéke=<%= entrop%> - <%=parameter%> paraméter esetén), és a Kolm-féle együttható (amelynek értéke=<%= kolm%> - <%=parameter%> paraméter esetén).
<%= emlekezteto <- 2 %>
<% } %>

<%= 
ineqs <- rbind(c("Gini-index", gni),
	c("Atkinson-index", round(atkinson,3)),
	c("Theil-index", round(theil,3)),
	c("Entrópia-index", round(entrop,3)),
	c("Kolm-index", round(kolm, 3)))
colnames(ineqs) <- c('Mutató', 'Értékek')
set.caption('Egyenlőtlenségi mutatók értékei táblázatosan') 
ineqs  
result_list[[szamlalo]][['ineqs']] <- ineqs 
result_list[[szamlalo]][['morani']] <-NA %>

### Az adatok térbeli eloszlásának autokorrelációs vizsgálata
<%=
plotvar.3 <- plotvar[match( HUN_kisterseg@data$NUTS4CODE, namevar)]
plotvar.3[which(plotvar.3 == Inf)] <- NA
plotvar.3[which(plotvar.3 == -Inf)] <- NA
moran_hiba  <- any(is.na(plotvar.3))
plotvar.3[which(is.na(plotvar.3))] <- mean(plotvar.3, na.rm = T)
%>
<%=
err <- try(szig_moran <- moran.mc(plotvar.3,
		col.W,
		nsim=999,
		zero.policy=FALSE,
		alternative="greater",
		spChk=NULL),
	TRUE)
%>
<% if (class(err) == "try-error" ) { %> 
**Az adatok strukturálódása nem teszi lehetővé az autokorrelációs vizsgálat végrehajtását!**
<% } else { %>
<%=
szig_moran <- moran.mc(plotvar.3,
	col.W,
	nsim = 999,
	zero.policy = FALSE,
	alternative = "greater",
	spChk = NULL)  
alsoCI <- round(sort(szig_moran $res)[25], 3)
felsoCI <- round(sort(szig_moran $res)[975], 3)
moran.limit <- round((-1 / (174 - 1)), 3) %>

* Az Moran féle I autokorrelációs mutató értéke <%= szig_moran$statistic %>, azaz a változó térben <%= if (szig_moran$statistic < moran.limit) {
		'negatívan'
	} else {
		'pozitívan' } %> <%= if (abs(szig_moran$statistic) < 0.3) {
			'de gyengén'
		} else {
			if (abs(szig_moran$statistic) < 0.5) {
				'és közepesen'
			} else {
				'és erősen'
			}} %> autokorrelált, tehát a véletlenszerű eloszláshoz viszonyítva <%=
if (szig_moran$statistic < moran.limit) {
	'*inkább alternáló, azaz sakktáblaszerű*'
} else {
	'*csomósodásra hajlamos*'
} %> a kistérségek értékeinek térbeli eloszlása. 

<%= if (moran_hiba) {
	'* **Fontos megjegyzés: A területi autokorreláció számításánál a térfolytonosság megőrzése
	érdekében a hiányzó, illetőleg a 0-val normalizálásból adódó értelmezhetetlen értékeket
	az átlag értékkel imputáltuk!**'
}%>
<%=
result_list[[szamlalo]][['morani']] <- round(szig_moran$statistic, 3)
evalsOptions('graph.unify', TRUE) %>

<%=   
if (length(plotvars)>1) {
    set.caption(paste0("A Moran's I statisztikához tartozó szomszédsági értékek eloszlása -", as.roman(ix), "."))
} else {
    set.caption("A Moran's I statisztikához tartozó szomszédsági értékek eloszlása")
}
moran.plot(plotvar.3, col.W, xlab="", ylab="" )
%>

<%= 
hist_i <- function() {
    hist(szig_moran$res, 20, main="", xlab="")
    abline(v=alsoCI, col="red") 
    abline(v=felsoCI, col="red")
}
if (length(plotvars)>1) {
    set.caption(paste0(
	"A Moran's I statisztikához tartozó Monte-Carlo szimulációs értékek eloszlása és a 95%-os konfidencia határok -",
	as.roman(ix),
	"."))
} else {

    set.caption("A Moran's I statisztikához tartozó Monte-Carlo szimulációs értékek eloszlása és a 95%-os konfidencia határok")
}
hist_i() %>

* A 174 kistérséghez tartozó tökéletes autokorrelálatlanságot jelentő elvi Moran érték <%= moran.limit %>
(a _-1/N-1_ [képlet alapján](http://geogr.elte.hu/REF/REF_Kiadvanyok/REF_RTT_10/RTT-10-10resz.pdf)). Ugyanakkor az adott eloszlás esetén az adott értékhez a hisztogrammon is látható konfidencia-intervallum tartozik amely értéktől az adatokból számított Moran fele I érték 95%-os valószínűséggel
<%=
if (!((alsoCI > szig_moran$statistic) | (felsoCI < szig_moran$statistic))) {
	'**nem** tér el.'
} else {
	'eltér.'
} %> A Moran mutatóhoz Monte-Carlo szimuláció révén számított 95%-os konfidencia intervallum határok
<%= paste0(alsoCI, '-', felsoCI) %> (ismétlések száma=_1000_), azaz a kapott eredmény **szignifikánsnak**
<%= if (!((alsoCI > szig_moran$statistic) | (felsoCI < szig_moran$statistic))) { '**nem**' } %> **tekinthető**!

### Az adatok lokális térbeli autokorrelációs értékei alapján homogén térségek azonosítása

<%= evalsOptions('graph.unify', FALSE) %>

<%=
locI <- localmoran(plotvar.3, col.W)
value_i <- locI[, 1]
szig_i <- locI[, 5]
value_ikat <- rep("Vegyes értékű környezet", length(szig_i))
value_ikat[which((value_i >= locI[, 2]) & (plotvar.3 > mean(plotvar.3, na.rm = T)))] <- "Magas-Magas környezet"
value_ikat[which((value_i >= locI[, 2]) & (plotvar.3 < mean(plotvar.3, na.rm = T)))] <- "Alacsony-Alacsony környezet"
value_ikat[which(szig_i>0.05)] <- "Nem szignifikáns"
id_i <- names(locI[, 1])

value_ikat <- as.character(value_ikat[match(HUN_kisterseg@data$NUTS4CODE, id_i)])
ikats <- sort(unique(as.character(value_ikat)))
colcode <- rep("#FFFFFF", length(value_ikat))

for (mi in 1:length(ikats)) {
    colcode[which(value_ikat==ikats[mi])] <- panderOptions('graph.colors')[mi]
}
attr(colcode, "palette") <- unique(colcode) 
attr(colcode, "table") <- unique(value_ikat)
    
if (length(ikats)>1) {
    if (length(plotvars)>1) {
        set.caption(paste0("A lokális Moran's I értékek alapján a különböző térbeli zónák azonosítása-",
		as.roman(ix), "."))
    } else {
        set.caption("A lokális Moran's I értékek alapján a különböző térbeli zónák azonosítása")
    }
    mapping2()
} else {
	'Nem azonosítható egyetlen egy statisztikai értelemben vett szignifánsnak nevezhető speciális régió sem.'
}
szamlalo <- szamlalo+1
%>

<% if (any(ikats=="Magas-Magas környezet")) { %>    
* A következő kistérségek helyezkednek el homogén magas értékű zónákban:
<%=
sort(as.character(HUN_kisterseg@data$NUTS4NAME[which(value_ikat=="Magas-Magas környezet")]))%>.
<% } %>
<% } %>
<% } %>
<% } else { %>
* Sajnáljuk, de a kért változóra ( <%= strwrap(rp.label(plotvar), 40) %>...) a kért feltételekkel nem lehetséges az elemzés végrehajtása, mivel - ahogyan az összegző táblából is kiolvasható,- a térképezni kívánt változó eloszlása nem teszi lehetővé, hogy elemezzük az eltéréseket, ugyanis nincs szórása!
<% } %>
<% } %>
<% if ((ix > 1) & (!onlymap)) { %>
## Összehasonlító elemzés
<%= 
set.caption('A vizsgált változók különböző mutatóinak összehasonlító táblázata') 
ineqss <- matrix(unlist(lapply(result_list, function(x) x[['ineqs']][,2])), ncol=nrow(result_list[[1]][['ineqs']]), byrow=TRUE)
kiir <- cbind(data.frame(
	változó=unlist(lapply(result_list, function(x) x[['név']])),
	stringsAsFactors=FALSE),
	ineqss,
	unlist(lapply(result_list, function(x) x[['morani']])))
colnames(kiir) <- c('Változó', result_list[[1]][['ineqs']][,1], "Moran's-I")
for (z in 1:nrow(kiir)) {
    kiir$Változó[z] <-  paste0(as.roman(z), '. változó- ', kiir$Változó[z])
}
attr(kiir, 'alignment') <- 'left'
kiir 
evalsOptions('graph.unify', TRUE)
%>
<%= 
lorenz_allin <- function() {
    plot(Lc(na.omit(result_list[[1]][['plotvar.2']])), main="", sub="", xlab="")
    title(main="Lorenz görbe")
	abline(0,1, col="red", lwd=2)
    for (lix in 2:length(plotvars)) {
    lines(Lc(na.omit(result_list[[lix]][['plotvar.2']])), col=panderOptions('graph.colors')[lix])
    }
    legend("topleft",
	paste0(as.character(as.roman(1:length(plotvars))), '. változó'),
	lty=c(rep(1, length(plotvars))),
	col=panderOptions('graph.colors')[1:length(plotvars)],
	text.col="black")
    }
set.caption(paste0('Összehasonlító grafikon - Lorenz görbék ',
	as.roman(1), '-',
	as.roman(length(plotvars)), '. változók'))
lorenz_allin()
evalsOptions('graph.unify', FALSE) %>
<%=
mini_values <- data.frame(
	Kistérség = unlist(lapply(result_list, function(x) x[['mini']]$Kistérség)),
	Érték = unlist(lapply(result_list, function(x) x[['mini']]$Érték)),
	stringsAsFactors = FALSE)
maxi_values <- data.frame(
	Kistérség = unlist(lapply(result_list, function(x) x[['maxi']]$Kistérség)),
	Érték = unlist(lapply(result_list, function(x) x[['maxi']]$Érték)),
	stringsAsFactors = FALSE) 
kotoszo <- FALSE
%>

<% if (max(table(mini_values$Kistérség)) > 1) { %>
<% if (length(names(which(table(mini_values$Kistérség) == max(table(mini_values$Kistérség)))))>1) { %>
* A vizsgált változók alapján a <%=
	sort(names(which(table(mini_values$Kistérség) == max(table(mini_values$Kistérség)))))
%> kistérségek emelkednek ki, mint többszörösen alacsony értékű területek.
<% } else { %>
* A vizsgált változók alapján a <%=
	names(which(table(mini_values$Kistérség) == max(table(mini_values$Kistérség))))
%> kistérség emelkedik ki, mint többszörösen alacsony értékű terület.
<% } %>
<%= kotoszo <- TRUE %>
<% } %>
<% if (max(table(maxi_values$Kistérség)) > 1) { %>

<%
if (length(names(which(
	table(maxi_values$Kistérség) == max(table(maxi_values$Kistérség)))))>1) {
%>
<% if (kotoszo) { %> * Továbbá a <% } else { %> A vizsgált változók alapján a <% } %><%=
	sort(names(which(table(maxi_values$Kistérség)==max(table(maxi_values$Kistérség)))))
%> kistérségek emelkednek ki, mint többszörösen magas értékű területek.
<% } else { %>
<% if (kotoszo) { %> * Továbbá a <% } else { %> A vizsgált változók alapján a <% } %><%=
	names(which(table(maxi_values$Kistérség)==max(table(maxi_values$Kistérség))))
%> kistérség emelkedik ki, mint többszörösen magas értékű terület.
<% } %>
<% } %>

<% if ((max(table(maxi_values$Kistérség)) == 1) & (max(table(mini_values$Kistérség)) == 1)) { %>
* Az vizsgálatba bevont változók alapján nem volt azonosítható töbszörösen kiemelkedő kistérség. 
<% } %>

* Egyenlőtlenségi szempontból a <%=
	as.character(as.roman(which(as.numeric(kiir[,2]) == max(as.numeric(kiir[,2]), na.rm=TRUE))[1]))
%>. változó esetén (<%=
	strwrap(result_list[[which(as.numeric(kiir[,2]) == max(as.numeric(kiir[,2]), na.rm=TRUE))[1]]][['név']], 60)[1]
%>...) találtuk a legnagyobb egyenlőtlenségeket, amelynek Gini értéke <%=
	if (max(as.numeric(kiir[,2])) < 0.4) {
		'ugyanakkor nem nevezhető szélsőségesnek.'
	} else {
		'erős egyenlőtlenségre utal.'
	} %> 
<% if (max(as.numeric(kiir[,7]), na.rm=TRUE)>0) { %>
* A Moran féle autokorrelációs értékek alapján elmondható, hogy leginkább a <%=
	as.character(as.roman(which(as.numeric(kiir[,7]) == max(as.numeric(kiir[,7]), na.rm=TRUE))[1]))
%>. változó (<%=
	strwrap(result_list[[which(as.numeric(kiir[,7]) == max(as.numeric(kiir[,7]), na.rm=TRUE))[1]]][['név']], 60)[1]
%>...) klasztereződik térben, <%
	if (max(as.numeric(kiir[,7]), na.rm=TRUE) < 0.3) {
%>amely ugyanakkor nem túl erős, azaz nem eredményez egyértelműen azonosítható térstrukturát. <%
	} else { %>amelynek mértéke olyan, ami már *látható* makrostruktúrát eredményez. <% } %> 
<% } %>


<% if (min(as.numeric(kiir[,7]), na.rm=TRUE) < -0.1) { %>
* A vizsgált változók közül különlegesnek nevezhető a <%=
	as.character(as.roman(which(as.numeric(kiir[,7]) == min(as.numeric(kiir[,7]), na.rm=TRUE))[1]))
%>. változó, mivel *negatív* az autokorrelációs értéke, azaz a térbeli strukturájára nem hogy térbeli sűrűsödés jellemző, hanem - társadalmi/gazdasági adatokra nem jellemző módon- a sakktáblához hasonló, alternáló jellegű.
<% } %>
<% } %>
<% } %>
