// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"Predicate":"rojo", "Class":"color","FemPredicate":"roja","copula":"estar"},
		{"Predicate":"amarillo", "Class":"color","FemPredicate":"amarilla","copula":"estar"},
		{"Predicate":"verde", "Class":"color","FemPredicate":"verde","copula":"estar"},
		{"Predicate":"azul", "Class":"color","FemPredicate":"azul","copula":"estar"},
		{"Predicate":"morado", "Class":"color","FemPredicate":"morado","copula":"estar"},
		{"Predicate":"marr&oacute;n", "Class":"color","FemPredicate":"marr&oacute;n","copula":"estar"},										
		{"Predicate":"grande", "Class":"size","FemPredicate":"grande","copula":"estar"},
		{"Predicate":"peque&ntilde;o", "Class":"size","FemPredicate":"peque&ntilde;a","copula":"estar"},				
		{"Predicate":"enorme", "Class":"size","FemPredicate":"enorme","copula":"estar"},
		{"Predicate":"min&uacute;sculo", "Class":"size","FemPredicate":"min&uacute;scula","copula":"estar"},					
		{"Predicate":"corto", "Class":"size","FemPredicate":"corto","copula":"estar"},
		{"Predicate":"largo", "Class":"size","FemPredicate":"larga","copula":"estar"},			
		// {"Predicate":"de madera", "Class":"material","FemPredicate":"de madera"},
		// {"Predicate":"pl&aacute;stico", "Class":"material","FemPredicate":"pl&aacute;stico"},
		// {"Predicate":"metal", "Class":"material","FemPredicate":"metal"},
		{"Predicate":"americano", "Class":"nationality","FemPredicate":"americana","copula":"ser"},
		{"Predicate":"mexicano", "Class":"nationality","FemPredicate":"mexicana","copula":"ser"},
		{"Predicate":"espa&ntilde;ol", "Class":"nationality","FemPredicate":"espa&ntilde;ola","copula":"ser"},
		{"Predicate":"liso", "Class":"texture","FemPredicate":"lisa","copula":"estar"},
		{"Predicate":"duro", "Class":"texture","FemPredicate":"dura","copula":"estar"},
		{"Predicate":"suave", "Class":"texture","FemPredicate":"suave","copula":"estar"},
		{"Predicate":"viejo", "Class":"age","FemPredicate":"vieja","copula":"estar"},
		{"Predicate":"nuevo", "Class":"age","FemPredicate":"nueva","copula":"estar"},
		{"Predicate":"podrido", "Class":"age","FemPredicate":"podrida","copula":"estar"},
		{"Predicate":"fresco", "Class":"age","FemPredicate":"fresca","copula":"estar"},
		{"Predicate":"bueno", "Class":"quality","FemPredicate":"buena","copula":"estar"},
		{"Predicate":"malo", "Class":"quality","FemPredicate":"mala","copula":"estar"},
		{"Predicate":"redondo", "Class":"shape","FemPredicate":"redonda","copula":"estar"},		
		{"Predicate":"cuadrado", "Class":"shape","FemPredicate":"cuadrada","copula":"estar"}
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

	for (var i=0; i<adjectives.length; i++) {
		noun = _.sample(nouns);
		stims.push(
			{
				"Predicate1":adjectives[i],
				"FemPredicate":adjectives[i].FemPredicate,
				"Class":adjectives[i].Class,	
				"Copula": adjectives[i].copula,			
				"Noun":noun.Noun,
				"NounGender":noun.Gender,
				"NounClass":noun.NounClass
			}
			);
		}
		
	return stims;
	
}