import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FilterMedicalRecordEntriesComponent } from '../filter-medical-record-entries/filter-medical-record-entries.component';
import { Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { PatientService } from '../../services/patient.service';
import { MedicalRecord, Patient, PatientsView } from '../../models/patient';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-list-medical-record-entries',
  standalone: true,
  imports: [CommonModule, FormsModule, FilterMedicalRecordEntriesComponent],
  templateUrl: './list-medical-record-entries.component.html',
  styleUrl: './list-medical-record-entries.component.css'
})
export class ListMedicalRecordEntriesComponent implements OnInit {
  successMessage: string | null = null;
  errorMessage: string | null = null;
  medicalRecord: MedicalRecord | undefined;
  patients: PatientsView[] = [];
  filteredPatients: PatientsView[] = [];
  searchQuery = '';
  selectedMedicalRecordNumber: string | null = null;
  selectedPatient: PatientsView | null = null;  // Store the selected patient

  constructor(private patientService: PatientService, private router: Router, private messageService: MessageService) { }

  ngOnInit(): void {
    this.successMessage = this.messageService.getMessage();
    this.patientService.getPatients().subscribe({
      next: (value: PatientsView[]) => {
        this.patients = value;
      },
      error: (err: any) => {
        console.error('Failed to get patients', err);
        this.errorMessage = 'Failed to get patients: ' + err.error.message;
      }
    });
    this.filteredPatients = [...this.patients];
  }

  filterMedicalRecords() {
    // Filters patients based on search query
    if (this.searchQuery === '') {
        this.filteredPatients = [...this.patients];  // Show all when input is empty
    } else {
        this.filteredPatients = this.patients.filter(patient =>
            patient.medicalRecordNumber.toLowerCase().includes(this.searchQuery.toLowerCase())
        );
    }
}

selectPatient(patient: PatientsView) {
    this.selectedMedicalRecordNumber = patient.medicalRecordNumber;
    this.selectedPatient = patient;  // Store the selected patient
    this.searchQuery = patient.medicalRecordNumber;  // Optionally, set search query to selected value
    this.filteredPatients = [patient];  // Show only the selected patient
}

onRecordNumberDeselected() {
    this.selectedMedicalRecordNumber = null;
    this.selectedPatient = null;  // Clear selected patient
    this.searchQuery = '';  // Reset search query when deselected
    this.filteredPatients = [...this.patients];  // Reset to show all patients
}

updateMedicalRecord() {
    // Logic for updating the medical record
    console.log("Update medical record for", this.selectedPatient);
}

  onFilterChanged(filter: { name: string }): void {
    if (!this.selectedMedicalRecordNumber) {
      this.patientService.filterMedicalRecordEntries(this.selectedMedicalRecordNumber, filter.name).subscribe({
        next: (value: MedicalRecord) => {
          this.medicalRecord = value;
        },
        error: (err: any) => {
          console.error('Failed to filter medical record entries', err);
        }
      });
    }
  }

}
