// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"Predicate":"rojo", "Class":"color","FemPredicate":"roja"},
		{"Predicate":"amarillo", "Class":"color","FemPredicate":"amarilla"},
		{"Predicate":"verde", "Class":"color","FemPredicate":"verde"},
		{"Predicate":"azul", "Class":"color","FemPredicate":"azul"},
		{"Predicate":"morado", "Class":"color","FemPredicate":"morado"},
		{"Predicate":"marrón", "Class":"color","FemPredicate":"marrón"},											
		{"Predicate":"grande", "Class":"size","FemPredicate":"grande"},
		{"Predicate":"pequeño", "Class":"size","FemPredicate":"pequeña"},					
		{"Predicate":"enorme", "Class":"size","FemPredicate":"enorme"},					
		{"Predicate":"minúsculo", "Class":"size","FemPredicate":"minúscula"},					
		{"Predicate":"corto", "Class":"size","FemPredicate":"corto"},					
		{"Predicate":"largo", "Class":"size","FemPredicate":"larga"},							
		{"Predicate":"de madera", "Class":"material","FemPredicate":"de madera"},
		{"Predicate":"plástico", "Class":"material","FemPredicate":"plástico"},
		{"Predicate":"metal", "Class":"material","FemPredicate":"metal"},
		{"Predicate":"liso", "Class":"texture","FemPredicate":"lisa"},
		{"Predicate":"duro", "Class":"texture","FemPredicate":"dura"},
		{"Predicate":"suave", "Class":"texture","FemPredicate":"suave"},
		{"Predicate":"viejo", "Class":"age","FemPredicate":"vieja"},
		{"Predicate":"nuevo", "Class":"age","FemPredicate":"nueva"},
		{"Predicate":"podrido", "Class":"age","FemPredicate":"podrida"},
		{"Predicate":"fresco", "Class":"age","FemPredicate":"fresca"},
		{"Predicate":"bueno", "Class":"quality","FemPredicate":"buena"},
		{"Predicate":"malo", "Class":"quality","FemPredicate":"mala"},
		{"Predicate":"redondo", "Class":"shape","FemPredicate":"redonda"},						
		{"Predicate":"cuadrado", "Class":"shape","FemPredicate":"cuadrada"}
]);

var nouns = [
		{"Noun":"manzana", "NounClass":"food", "Gender": "feminine"},
		{"Noun":"plátano", "NounClass":"food", "Gender":"masculine"}, 
		{"Noun":"zanahoria", "NounClass":"food", "Gender": "feminine"},
		{"Noun":"queso", "NounClass":"food", "Gender":"masculine"},
		{"Noun":"tomate", "NounClass":"food", "Gender": "masculine"},								
		{"Noun":"silla", "NounClass":"furniture", "Gender": "feminine"},								
		{"Noun":"sofá ", "NounClass":"furniture", "Gender": "masculine"},								
		{"Noun":"ventilador", "NounClass":"furniture", "Gender": "masculine"},								
		{"Noun":"televisión ", "NounClass":"furniture", "Gender": "feminine"},								
		{"Noun":"escritorio", "NounClass":"furniture", "Gender": "masculine"}								
];

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	while (stims.length < 26) {
		noun = _.sample(nouns);
		pred1 = _.sample(adjectives);
		pred2 = _.sample(adjectives);
		if (pred1.Class!=pred2.Class) {
			stims.push(
				{
					"Predicate1":pred1.Predicate,
					"Class1":pred1.Class,	
					"Predicate2":pred2.Predicate,
					"Class2":pred2.Class,			
					"Noun":noun.Noun,
					"NounClass":noun.NounClass
				}			
			);
		}
	}
		
	return stims;
	
}