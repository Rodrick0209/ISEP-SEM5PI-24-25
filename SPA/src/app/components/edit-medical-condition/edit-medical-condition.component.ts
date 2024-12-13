import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { MedicalCondtionService } from '../../services/medicalConditions.service';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-edit-medical-condition',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './edit-medical-condition.component.html',
  styleUrl: './edit-medical-condition.component.css'
})
export class EditMedicalConditionComponent {
  submitForm = {
    name: '',
  }

  medicalConditionName: string | null = null;
  errorMessage: string = '';
  showConfirmation: boolean = false;

  constructor(private router: Router, private medicalConditionService: MedicalCondtionService, private route: ActivatedRoute, private messageService: MessageService) { }

  ngOnInit(): void {
    this.medicalConditionName = this.route.snapshot.paramMap.get('medicalConditionName');

    let medicalCondition = null;
    if (this.medicalConditionName != null) {
      medicalCondition = this.medicalConditionService.getMedicalConditionCatalogItem(this.medicalConditionName);
      medicalCondition.subscribe({
        next: (data) => {
          this.submitForm.name = data.name;
        },
        error: (err) => console.error('Error loading medical condition', err)
      });
    }
  }

  confirmSubmission(): void {
    this.showConfirmation = true;
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false;
  }

  onSubmit(patientForm: any): void {
    this.showConfirmation = false;
    if (this.medicalConditionName) {
      this.medicalConditionService.updateMedicalConditionCatalogItem(
        this.medicalConditionName,
        this.submitForm.name
      ).subscribe(
        (response) => {
          console.log("Allergy edited successfully", response);
          this.messageService.setMessage(`Medical Condition ${this.medicalConditionName} successfully edited!`);
          this.router.navigate(['/medicalConditions']);
        },
        (error) => {
          this.errorMessage = error.error.message;
          console.error("Failed to edit allergy", error);
        }
      );
    } else {
      this.errorMessage = 'Medical condition name is missing.';
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/medicalConditions']);
  }
}
