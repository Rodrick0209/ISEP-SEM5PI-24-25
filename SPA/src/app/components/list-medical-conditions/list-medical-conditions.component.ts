import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { MedicalCondition } from '../../models/medicalCondition';
import { MedicalCondtionService } from '../../services/medicalConditions.service';



@Component({
  selector: 'app-list-medical-conditions',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './list-medical-conditions.component.html',
  styleUrl: './list-medical-conditions.component.css'
})
export class ListMedicalConditionsComponent {
    medicalConditions: MedicalCondition[] = [];
    isLoading = false;
    message: string = '';

    constructor(private router: Router, private medicalConditionsService: MedicalCondtionService) { }

    ngOnInit(): void {
      this.message = '';
      this.isLoading = true;
      this.medicalConditionsService.getMedicalConditions().subscribe({
        next: (data: MedicalCondition[]) => {
          this.isLoading = false;
          this.medicalConditions = data;
        },
        error: (err: any) => {
          console.error('Failed to fetch medical conditions', err);
          this.isLoading = false;
          this.medicalConditions = [];
          this.message = 'An error occurred while fetching medical conditions: ';
        }
      });
    }




    addMedicalCondition() {
      this.router.navigate(['/medicalConditions/add']);
    }

    editMedicalCondition(medicalCondition: MedicalCondition) {
      this.router.navigate(['/medicalConditions/edit', medicalCondition.name]);
    }

}
