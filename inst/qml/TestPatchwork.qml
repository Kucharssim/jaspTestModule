import QtQuick 			2.8
import JASP.Controls 	1.0
import JASP.Widgets		1.0

Form
{

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"								}
		AssignedVariablesList	{ name: "variables";		title: qsTr("Variables")	}
	}

	Section
	{
		title: qsTr("Simulation")

		CheckBox {	name: "runSimulation";	label: qsTr("Run simulation")	}
		IntegerField {	name: "sampleSize"; label: qsTr("Sample size"); defaultValue: 100 }
		RadioButtonGroup
		{
			title:	qsTr("Engine")
			name:	"engine"
			RadioButton {	name: "gridExtra";	label: "gridExtra"	}
			RadioButton {	name: "patchwork";	label: "patchwork";	id: patchwork}
		}

		RadioButtonGroup
		{
			title:	qsTr("Layout")
			name:	"layout"
			RadioButton
			{
				name: "grid"
				label: qsTr("Grid")
				IntegerField {	name: "gridRows";		label: qsTr("Number of rows")		}
				IntegerField {	name: "gridColumns";	label: qsTr("Number of columns")	}
			}
			RadioButton
			{
				name: "spiral";
				label: qsTr("Spiral")
				IntegerField	{	name: "spiralSize";		label: qsTr("Number of plots")	}
				CheckBox		{	name: "spiralNested";	label: qsTr("Nested (patchwork only)");	enabled: patchwork.checked	}
			}
		}
	}
}
