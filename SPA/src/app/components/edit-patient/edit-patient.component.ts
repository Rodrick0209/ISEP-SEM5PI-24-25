import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, NavigationExtras, Router } from '@angular/router';
import { PatientService } from '../../services/patient.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-edit-patient',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './edit-patient.component.html',
  styleUrl: './edit-patient.component.css'
})
export class EditPatientComponent implements OnInit {
  submitForm = {
    name: '',
    email: '',
    phoneNumber: '',
    street: '',
    postalCode: '',
    city: '',
    country: '',
  }

  medicalRecordNumber: string | null = null;
  errorMessage: string = '';
  showConfirmation: boolean = false;
  
  constructor(private router: Router, private patientService: PatientService, private route: ActivatedRoute, private messageService: MessageService) { }

  ngOnInit(): void {
    this.medicalRecordNumber = this.route.snapshot.paramMap.get('medicalRecordNumber');
  }

  confirmSubmission(): void {
    this.showConfirmation = true;
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false;
  }

  onSubmit(patientForm: any): void {
    this.showConfirmation = false;
    if (patientForm.valid) {
      const patientData = {
        ...this.submitForm,
      };

      if (this.medicalRecordNumber) {
        this.patientService.editPatient(
          this.medicalRecordNumber,
          patientData.name,
          patientData.email,
          patientData.phoneNumber,
          patientData.street,
          patientData.postalCode,
          patientData.city,
          patientData.country,
        ).subscribe(
          (response) => {
            console.log("Patient edited successfully", response);
            this.messageService.setMessage(`Patient nº ${this.medicalRecordNumber} successfully edited!`);
            this.router.navigate(['/patients']);
          },
          (error) => {
            this.errorMessage = error.error.message;
            console.error("Failed to edit patient", error);
          }
        );
      } else {
        this.errorMessage = 'Medical record number is missing.';
      }
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/patients']);
  }
}
