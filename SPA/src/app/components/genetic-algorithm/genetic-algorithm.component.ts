import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { PlanningService, ScheduleGeneticResult } from '../../services/planning.service';
import { Router } from '@angular/router';

@Component({
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './genetic-algorithm.component.html',
  styleUrl: './genetic-algorithm.component.css'
})
export class GeneticAlgorithmComponent {
    populationSize: number = 0;
    generations: number = 0;
    mutationRate: number = 0;
    crossoverRate: number = 0;
    bestIndividualToBeKeptRate: number = 0;
    lowerCostWanted: number = 0;
    timeLimit: number = 0;
    date: string = '';
    errorMessage: string = '';
    isLoading: boolean = false;
    today: string = '';
    scheduleResults : ScheduleGeneticResult | null = null;

    constructor(private planningService: PlanningService,private router: Router ) {
     }


     ngOnInit() {
        const currentDate = new Date();
        const year = currentDate.getFullYear();
        const month = (currentDate.getMonth() + 1).toString().padStart(2, '0');
        const day = currentDate.getDate().toString().padStart(2, '0');
        this.today = `${year}-${month}-${day}`;
      }

    


     agendarCirurgias() {
      if (this.populationSize && this.generations && this.mutationRate && this.crossoverRate && this.bestIndividualToBeKeptRate && this.lowerCostWanted && this.timeLimit && this.date) {
        this.planningService.geneticAlgorithm(this.populationSize, this.generations, this.mutationRate, this.crossoverRate, this.bestIndividualToBeKeptRate, this.lowerCostWanted, this.timeLimit, this.date).subscribe({
          next: (data) => {
            console.log('Schedule received from the Planning:', data);
            this.isLoading = false;
            this.scheduleResults = data;
            this.router.navigate(['/schedule-results'], { state: { schedule: data } });
          },
          error: (error) => {
            this.errorMessage = 'It wasnt possible to schedule the surgeries. Please try again with other parameters or contact the admin.';
            this.isLoading = false;
          } 
        });
      }else {
        this.errorMessage = 'Please fill all the fields before trying to schedule the surgeries.';
      }
     }

     closeScheduleBox() {
      this.scheduleResults = null;
      }




     goBack() {
      this.router.navigate(['/planning']);
     }


}
