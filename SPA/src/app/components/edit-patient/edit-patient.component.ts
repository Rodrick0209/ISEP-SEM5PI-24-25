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
    allergies: [{ name: '', date: '', description: '' }],
    medicalConditions: [{ name: '', date: '', description: '' }]

  }

  allergyOptions: string[] = [];
  conditionOptions: string[] = [];


  medicalRecordNumber: string | null = null;
  errorMessage: string = '';
  showConfirmation: boolean = false;

  constructor(private router: Router, private patientService: PatientService, private route: ActivatedRoute, private messageService: MessageService) { }

  ngOnInit(): void {
    this.medicalRecordNumber = this.route.snapshot.paramMap.get('medicalRecordNumber');

    let patient = null;
    if (this.medicalRecordNumber != null) {
      patient = this.patientService.getPatientByMedicalRecordNumber(this.medicalRecordNumber);
      patient.subscribe({
        next: (data) => {
          this.submitForm.name = data.name;
          this.submitForm.email = data.email;
          this.submitForm.phoneNumber = data.phoneNumber;
          this.submitForm.street = data.address.street;
          this.submitForm.country = data.address.country;
          this.submitForm.postalCode = data.address.postalCode;
          this.submitForm.city = data.address.city;
        },
        error: (err) => console.error('Error loading patient', err)
      });
    }

    let medRec;
    if (this.medicalRecordNumber != null) {
      medRec = this.patientService.getMedicalRecordByPatientId(this.medicalRecordNumber);

      medRec.subscribe({
        next: (data) => {
          console.log('Medical record data:', data.allergies);
          this.submitForm.allergies = data.allergies.map((allergy: any) => ({
            name: allergy,       // Assign string to 'name'
            date: '',            // Default empty date
            description: ''      // Default empty description
          }));
        },
        error: (err) => console.error('Error loading patient', err)
      });

    }

    this.patientService.getAllAllergies().subscribe({
      next: (data) => {
        this.allergyOptions = data.map(allergy => allergy.name);
      },
      error: (err) => console.error('Error loading allergies', err)
    });

    // Carrega opções de condições médicas
    this.patientService.getAllAllMedicalConditions().subscribe({
      next: (data) => {
        this.conditionOptions = data.map(condition => condition.name);
      },
      error: (err) => console.error('Error loading medical conditions', err)
    });

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
          patientData.allergies
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

  // Methods for Allergies
  addAllergy(): void {
    this.submitForm.allergies.push({ name: '', date: '', description: '' });
  }

  removeAllergy(index: number): void {
    this.submitForm.allergies.splice(index, 1);
  }

  // Methods for Medical Conditions
  addCondition(): void {
    this.submitForm.medicalConditions.push({ name: '', date: '', description: '' });
  }

  removeCondition(index: number): void {
    this.submitForm.medicalConditions.splice(index, 1);
  }
}
