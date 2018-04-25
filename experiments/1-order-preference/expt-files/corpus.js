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
		{"Predicate":"marr&oacute;n", "Class":"color","FemPredicate":"marr&oacute;n"},											
		{"Predicate":"grande", "Class":"size","FemPredicate":"grande"},
		{"Predicate":"peque&ntilde;o", "Class":"size","FemPredicate":"peque&ntilde;a"},					
		{"Predicate":"enorme", "Class":"size","FemPredicate":"enorme"},					
		{"Predicate":"min&uacute;sculo", "Class":"size","FemPredicate":"min&uacute;scula"},					
		{"Predicate":"corto", "Class":"size","FemPredicate":"corto"},					
		{"Predicate":"largo", "Class":"size","FemPredicate":"larga"},							
		{"Predicate":"de madera", "Class":"material","FemPredicate":"de madera"},
		{"Predicate":"pl&aacute;stico", "Class":"material","FemPredicate":"pl&aacute;stico"},
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
		{"Noun":"pl&aacute;tano", "NounClass":"food", "Gender":"masculine"}, 
		{"Noun":"zanahoria", "NounClass":"food", "Gender": "feminine"},
		{"Noun":"queso", "NounClass":"food", "Gender":"masculine"},
		{"Noun":"tomate", "NounClass":"food", "Gender": "masculine"},								
		{"Noun":"silla", "NounClass":"furniture", "Gender": "feminine"},								
		{"Noun":"sof&aacute; ", "NounClass":"furniture", "Gender": "masculine"},								
		{"Noun":"ventilador", "NounClass":"furniture", "Gender": "masculine"},								
		{"Noun":"televisi&oacute;n ", "NounClass":"furniture", "Gender": "feminine"},								
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
					"Predicate1":pred1,
					"Class1":pred1.Class,	
					"Predicate2":pred2,
					"Class2":pred2.Class,			
					"Noun":noun.Noun,
					"NounClass":noun.NounClass,
					"NounGender":noun.Gender
				}			
			);
		}
	}
		
	return stims;
	
}