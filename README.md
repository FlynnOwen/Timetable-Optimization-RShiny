# Timetable-Optimization-RShiny
Exams Made Easy (EME) is designed to help high schools with scheduling timetables for exams, as well as helping to provide accessible insights into which classes students are taking. The timetable scheduling problem is defined as finding the minimum amount of timeslots needed to create a timetable, such that there are no clashes between classes for any one student. Timetable scheduling is recognised as an especially difficult problem, and is classified as NP-hard. Given that this problem is NP-hard, there are naturally many different methods of optimisation. Of these methods recognised, genetic algorithms have been known to produce the best results. However another effective known solution is graph colouring. The nodes in a network can be represented by classes, and the relations between these nodes (edges) are if at least one student is taking both of these classes. After the graph is plotted, the graph can be coloured in a way such that no two adjacent nodes are the same colour. Colours can represent time-slots for an examination timetable - as no two classes could be scheduled to be examined at the same time. Finding the minimum number of colours needed to do this then finds the minimum number of timeslots needed to create an examination timetable without any clashes. This colouring is also referred to as the "chromatic" colouring of the graph.

To run the project:
* Download the file shinyapp
* Open file ui.R or server.R
* Click run.app

- Note that under folder R_Scripts a dataset can be generated to run through the application if the user would like to see how the application performs on a new dataset.
