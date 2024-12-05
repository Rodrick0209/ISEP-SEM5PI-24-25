import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { MedicalCondtionService } from '../../services/medicalConditions.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-create-medical-conditions',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './create-medical-conditions.component.html',
  styleUrl: './create-medical-conditions.component.css'
})
export class CreateMedicalConditionsComponent {
    medicalConditionToCreate: string = '';
    isLoading = false;
    message: string = '';
    showConfirmation: boolean = false;


    constructor(private router:Router, private medicalConditionService:MedicalCondtionService) { }

    confirmSubmission(): void {
      this.showConfirmation = true;
    }

    closeConfirmationModal(): void {
      this.showConfirmation = false;
    }

    onSubmit(medicalConditionToCreate: any): void {
      this.isLoading = true;
      this.showConfirmation = false;

      if (medicalConditionToCreate.valid) {

        const name = this.capitalizeFirstLetter(medicalConditionToCreate.value.name);

        this.medicalConditionService.createMedicalCondition(name).subscribe({
          next: (data: any) => {
            this.isLoading = false;
            this.router.navigate(['/medicalConditions']);
          },
          error: (err: any) => {
            this.isLoading = false;
            this.message = "Failed to create medical condition";
          }
        });
      }

    
    }

    private capitalizeFirstLetter(input: string): string {
      if (!input) return input; // Retorna vazio se a entrada for inválida
      return input.charAt(0).toUpperCase() + input.slice(1).toLowerCase();
    }
  
  
    onCancel(): void {
      this.showConfirmation = false;
      this.router.navigate(['/medicalConditions']);
    }
  

}
