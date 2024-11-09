import { Component } from '@angular/core';
import { NavigationExtras, Router } from '@angular/router';
import { PatientService } from '../../services/patient.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-create-patient',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './create-patient.component.html',
  styleUrls: ['./create-patient.component.css'] // Ensure this is plural
})
export class CreatePatientComponent {
  
  
  submitForm = {
    firstName: '',
    lastName: '',
    fullName: '',
    dateOfBirth: '',
    gender: '',
    email: '',
    phoneNumber: '',
    street: '',
    postalCode: '',
    city: '',
    country: '',
    emergencyContactName: '',
    emergencyContactEmail: '',
    emergencyContactPhoneNumber: ''
  }

  showConfirmation: boolean = false;
  errorMessage: string = '';

  constructor(private router: Router, private patientService: PatientService, private messageService: MessageService) { }

  confirmSubmission(): void {
    this.showConfirmation = true; // Show confirmation modal
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false; // Close the confirmation modal
  }

  onSubmit(patientForm: any): void {
    this.showConfirmation = false;
    if (patientForm.valid) {
      const patientData = {
        ...this.submitForm,
        dateOfBirth: new Date(this.submitForm.dateOfBirth).toISOString()
      };

      this.patientService.createPatient(
        patientData.firstName,
        patientData.lastName,
        patientData.fullName,
        patientData.dateOfBirth,
        patientData.gender,
        patientData.email,
        patientData.phoneNumber,
        patientData.street,
        patientData.postalCode,
        patientData.city,
        patientData.country,
        patientData.emergencyContactName,
        patientData.emergencyContactEmail,
        patientData.emergencyContactPhoneNumber
      ).subscribe({
        next: (data: any) => {
          console.log('Patient created successfully', data);
          this.messageService.setMessage(`Patient ${patientData.fullName} successfully created!`);
          this.router.navigate(['/patients']);
        },
        error: (err: any) => {
          console.log('Failed to create patient', err);
          this.errorMessage = 'Failed to create patient: ' + (err.error?.message || 'Unknown error');
        }
      });
    } else {
      console.log('Form is invalid');
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/patients']);
  }
}
