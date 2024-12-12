import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FilterMedicalRecordEntriesComponent } from '../filter-medical-record-entries/filter-medical-record-entries.component';
import { DatePipe } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { PatientService } from '../../services/patient.service';
import { Allergy, MedicalRecord, Patient, PatientsView, AllergiesView, MedicalConditionView, MedicalCondition } from '../../models/patient';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-edit-medical-record',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './edit-medical-record.component.html',
  styleUrl: './edit-medical-record.component.css',
  providers: [DatePipe] // Add DatePipe to the providers array

})
export class EditMedicalRecordComponent {
  successMessage: string | null = null;
  errorMessage: string | null = null;
  medicalRecord: MedicalRecord | undefined;
  medicalRecordNumber: string = '';
  existingAllergies: AllergiesView[] | [] = [];
  existingMedicalConditions: MedicalConditionView[] | [] = [];

  constructor(private patientService: PatientService, private router: Router, private messageService: MessageService, private route: ActivatedRoute, private datePipe: DatePipe) { }

  ngOnInit(): void {
    this.successMessage = this.messageService.getMessage();
    this.medicalRecordNumber = this.route.snapshot.paramMap.get('medicalRecordNumber') || ''
    this.getMedicalRecord();
    this.getAllAllergies();
    this.getAllMedicalConditions();
  }

  getMedicalRecord(): void {
    if (this.medicalRecordNumber) {
      this.patientService.getMedicalRecordByPatientId(this.medicalRecordNumber).subscribe({
        next: (record: MedicalRecord) => {
          this.medicalRecord = record;
        },
        error: (err: any) => {
          console.error('Failed to get medical record', err);
          this.errorMessage = 'Failed to get medical record';
        }
      });
    }
  }

  updateMedicalRecord() {
    if (this.medicalRecordNumber) {
      this.router.navigate(['/edit-medical-record', this.medicalRecordNumber]);
    }

  }


  addAllergy(): void {
    const newAllergy: Allergy = { id: '', name: '', description: '' };
    if (this.medicalRecord) {
      this.medicalRecord.allergies.push(newAllergy);
    }
  }

  addMedicalCondition(): void {
    const newCondition: MedicalCondition = { id: '', name: '', date: new Date() };
    if (this.medicalRecord) {
      this.medicalRecord.medicalConditions.push(newCondition);
    }
  }

  saveAll(): void {
    if (this.medicalRecord) {
      this.patientService.updateMedicalRecord(this.medicalRecordNumber, this.medicalRecord.allergies, this.medicalRecord.medicalConditions).subscribe({
        next: () => {
          this.successMessage = 'Medical record updated successfully!';
          this.errorMessage = null;
          this.router.navigate(['/patient/medical-record', this.medicalRecordNumber]); // Move navigation inside the success callback
        },
        error: (err: any) => {
          console.error('Failed to save medical record', err);
          this.errorMessage = 'Failed to save medical record';
          this.successMessage = null;
        }
      });
    }
  }

  cancelAll(): void {
    if (this.medicalRecordNumber) {
      this.router.navigate(['/patient/medical-record', this.medicalRecordNumber]); // Navigate to the patient's medical record page
    }
  }

  deleteAllergy(allergy: { name: string, description: string }): void {
    if (this.medicalRecord) {
      const index = this.medicalRecord.allergies.findIndex(a => a.name === allergy.name && a.description === allergy.description);
      if (index > -1) {
        this.medicalRecord.allergies.splice(index, 1);
      }
    }
  }

  deleteCondition(condition: { name: string, date: Date }): void {
    if (this.medicalRecord) {
      const index = this.medicalRecord.medicalConditions.findIndex(a => a.name === condition.name && a.date === condition.date);
      if (index > -1) {
        this.medicalRecord.medicalConditions.splice(index, 1);
      }
    }
  }



  getAllAllergies(): void {
    this.patientService.getAllAllergies().subscribe({
      next: (allergies: AllergiesView[]) => {
        this.existingAllergies = allergies;
      },
      error: (err: any) => {
        console.error('Failed to get allergies', err);
        this.errorMessage = 'Failed to get allergies';
      }
    });
  }

  getAllMedicalConditions(): void {
    this.patientService.getAllAllMedicalConditions().subscribe({
      next: (medicalCondition: MedicalConditionView[]) => {
        this.existingMedicalConditions = medicalCondition;
      },
      error: (err: any) => {
        console.error('Failed to get allergies', err);
        this.errorMessage = 'Failed to get allergies';
      }
    });
  }

  // Format full ISO date to 'YYYY-MM-DD' for <input type="date">
  formatDateForInput(date: Date | null): string | null {
    if (!date) return null; // Handle null or empty dates
    return this.datePipe.transform(date, 'yyyy-MM-dd'); // Use DatePipe to format the date
  }

  // Update the condition date back to ISO format
  updateConditionDate(newDate: string, condition: any): void {
    condition.date = new Date(newDate).toISOString(); // Convert 'YYYY-MM-DD' back to ISO
  }
}
