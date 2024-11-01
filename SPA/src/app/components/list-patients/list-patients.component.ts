import { Component, OnInit } from '@angular/core';
import { PatientsView, PatientService } from '../../services/patient.service';
import { Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { FilterPatientsComponent } from '../filter-patients/filter-patients.component';

@Component({
  selector: 'app-list-patients',
  standalone: true,
  imports: [CommonModule, FilterPatientsComponent],
  templateUrl: './list-patients.component.html',
  styleUrl: './list-patients.component.css'
})
export class ListPatientsComponent implements OnInit {
  patients: PatientsView[] = [];
  filteredPatients: PatientsView[] = [];

  constructor(private patientService: PatientService, private router: Router) { }

  ngOnInit(): void {
    this.patientService.getPatients().subscribe({
      next: (data: PatientsView[]) => {
        this.patients = data;
        this.filteredPatients = data;
      },
      error: (err: PatientsView[]) => {
        console.error('Failed to fetch patients', err);
        this.filteredPatients = []; // Clear the list on error
      }
    });
  }

  onFilterChanged(filter: { medicalRecordNumber: string, name: string, email: string, dateOfBirth: string }): void {
    this.patientService.filterPatients(filter.medicalRecordNumber, filter.name, filter.dateOfBirth, filter.email).subscribe({
      next: (data: PatientsView[]) => this.filteredPatients = data,
      error: (err: PatientsView[]) => {
        console.error('Failed to filter patients', err);
        this.filteredPatients = []; // Clear the list on error
      }
    });
  }

  seeDetails(patient: PatientsView): void {
    // Navigate to the patient details page
    this.router.navigate(['/patient-details', patient.medicalRecordNumber]);
    console.log('Details of patient:', patient);
  }

  editPatient(patient: PatientsView): void {
    // Navigate to the edit patient page
    console.log('Editing patient:', patient);
  }

  deletePatient(patient: PatientsView): void {
    // Implement the delete patient functionality
  }

}
