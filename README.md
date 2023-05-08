# BioLumeID
Shiny application in R that allows for identifying whether a searched organism is bioluminescent

BioLumeID est une application conçue pour permettre l'identification d'espèces bioluminescentes. Elle utilise les données d'Oba 2021 et de Martini S. and Haddock 2017.


Le fichier Oba 2021 recense toutes les espèces bioluminescentes connues à ce jour, tandis que le fichier Martini S. et Haddock 2017 répertorie des concepts identifiés à des profondeurs différentes, avec leur niveau de description le plus précis et leur qualité de bioluminescence correspondante (expliqué plus loin).

La première section de l'application vous permettra de vérifier si une espèce, un taxon ou un genre est bioluminescent en affichant les données de deux tableaux de données issus de ces bases de données."

Si la recherche ne donne aucun résultat, cela signifie que l'information n'est pas présente dans les données, que votre recherche est incorrecte, que l'organisme n'est pas bioluminescent ou n'a pas encore été répertorié comme tel.

- Dans le premier tableau, vous pourrez filtrer les différentes catégories de bioluminescence, telles que bioluminescentes, probablement bioluminescentes, non définies, probablement pas bioluminescentes et non-bioluminescentes."

- Dans le deuxième tableau, vous pourrez afficher des informations complémentaires contenues dans le fichier Oba 2021
Dans ce tableau de données, certaines informations ont été écrites en japonais et aucune traduction ne sera effectuée afin d'éviter toute erreur de traduction. Si vous souhaitez consulter ces informations supplémentaires, nous vous invitons à vous référer au fichier source.


Dans la deuxième section de l'application, vous pourrez observer les graphiques effectués dans l'article de Martini S. and Haddock 2017 et modifier les paramètres sélectionnés. "
Toutefois, il est conseillé de ne jamais désélectionner tous les taxons, car cela entraînerait une erreur.


Enfin, dans la dernière partie de l'application, vous pourrez Uploader un tableau de données contenant vos identifications d'espèces ou de concepts.
L'application identifiera les informations de bioluminescence dans vos données et vous fournira un graphique représentant les proportions de bioluminescence dans vos données.
