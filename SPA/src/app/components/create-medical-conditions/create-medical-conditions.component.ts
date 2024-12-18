import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { MedicalCondtionService } from '../../services/medicalConditions.service';
import { Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { MedicalConditionCatalog } from '../../models/medicalConditionCatalog';

@Component({
  selector: 'app-create-medical-conditions',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './create-medical-conditions.component.html',
  styleUrl: './create-medical-conditions.component.css'
})
export class CreateMedicalConditionsComponent {
  submitForm = {
    code: '',
    designation: '',
    description: '',
  };

  commonSymptoms: string[] = [];
  newSymptom: string = '';

  errorMessage: string = '';
  successMessage: string = '';
  showConfirmation: boolean = false;

  constructor(private router:Router, private medicalConditionService:MedicalCondtionService, private messageService:MessageService) { }

  addSymptom(): void {
    if (this.newSymptom.trim()) {
      this.commonSymptoms.push(this.newSymptom.trim());
      this.newSymptom = ''; // Clear input after adding
    } else {
      this.errorMessage = 'Symptom cannot be empty';
    }
  }

    confirmSubmission(): void {
      this.showConfirmation = true;
    }

    closeConfirmationModal(): void {
      this.showConfirmation = false;
    }

    onSubmit(form: any): void {
      this.showConfirmation = false;

      if (form.valid) {
        this.medicalConditionService.createMedicalCondition(this.submitForm.code, this.submitForm.designation, this.submitForm.description, this.commonSymptoms).subscribe({
          next: (data: MedicalConditionCatalog) => {
            this.messageService.setMessage(`Medical Condition ${data.designation} successfully created!`);
            this.router.navigate(['/medicalConditions']);
          },
          error: (err: any) => {
            console.error('Failed to create medical condition', err);
            this.errorMessage = 'An error occurred while creating the medical condition: ' + err.error.message;
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
