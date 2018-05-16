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
		{"Predicate":"grande", "Class":"size","FemPredicate":"enorme"},
		{"Predicate":"peque&ntilde;o", "Class":"size","FemPredicate":"peque&ntilde;a"},					
		{"Predicate":"enorme", "Class":"size","FemPredicate":"enorme"},					
		{"Predicate":"min&uacute;sculo", "Class":"size","FemPredicate":"min&uacute;scula"},					
		{"Predicate":"corto", "Class":"size","FemPredicate":"corto"},					
		{"Predicate":"largo", "Class":"size","FemPredicate":"larga"},							
		{"Predicate":"de madera", "Class":"material","FemPredicate":"de madera"},
		{"Predicate":"de pl&aacute;stico", "Class":"material","FemPredicate":"de pl&aacute;stico"},
		{"Predicate":"de metal", "Class":"material","FemPredicate":"de metal"},
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
		{"Noun":"queso", "NounClass":"food", "Gender": "masculine"},
		{"Noun":"cabello", "NounClass":"body", "Gender": "masculine"},				
		{"Noun":"ojos", "NounClass":"body", "Gender": "masculine"},
		{"Noun":"cosa", "NounClass":"thing", "Gender": "feminine"}									
];

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	for (var i=0; i<adjectives.length; i++) {
		noun = _.sample(nouns);
		stims.push(
			{
				"Predicate":adjectives[i].Predicate,
				"Class":adjectives[i].Class,				
				"Noun":noun.Noun,
				"NounClass":noun.NounClass
			}
			);
		}
		
	return stims;
	
}