import QtQuick 			2.8
import QtQuick.Layouts 	1.3
import JASP.Controls 	1.0
import JASP.Widgets 	1.0
import JASP				1.0

Form
{
	columns: 1

	VariablesForm
	{
		preferredHeight: 520 * preferencesModel.uiScale
		AvailableVariablesList		{ name: "allVariablesList" }
		FactorLevelList
		{
			name: "repeatedMeasuresFactors"
			title: qsTr("Repeated Measures Factors")
			height: 180 * preferencesModel.uiScale
			factorName: qsTr("RM Factor")
		}
		AssignedRepeatedMeasuresCells
		{
			name:				"repeatedMeasuresCells"
			title:				qsTr("Repeated Measures Cells")
			source:				"repeatedMeasuresFactors"
		}
		AssignedVariablesList
		{
			name:				"betweenSubjectFactors"
			title:				qsTr("Between Subject Factors")
			suggestedColumns:	["ordinal", "nominal"]
			itemType:			"fixedFactors"
		}
	}

	TextArea
	{
		name:				"syntax"
		textType:			JASP.TextTypeRestriktor
		trim:				true
		applyScriptInfo:	qsTr("Ctrl + Enter to apply. Write stuff like 'RM Factor 1', 'Level 1', or 'contBinom'")
	}

	TabView
	{
		id:					models
		name:				"models"
		newItemName:		qsTr("Model 1")
		optionKey:			"modelName"

		content: Group{
			TextArea
			{
				name:				"syntax"
				width:				models.width
				textType:			JASP.TextTypeRestriktor
				trim:				true
				applyScriptInfo:	qsTr("Ctrl + Enter to apply. Write stuff like 'RM Factor 1', 'Level 1', or 'contBinom'")
			}
		}
	}
}
