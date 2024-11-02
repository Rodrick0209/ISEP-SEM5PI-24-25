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
  paginatedPatients: PatientsView[] = [];

  currentPage: number = 1;
  itemsPerPage: number = 10;
  totalPages: number = 0;

  constructor(private patientService: PatientService, private router: Router) { }

  ngOnInit(): void {
    this.patientService.getPatients().subscribe({
      next: (data: PatientsView[]) => {
        this.patients = data;
        this.filteredPatients = data;
        this.totalPages = Math.ceil(this.filteredPatients.length / this.itemsPerPage);
        this.updatePagination();
      },
      error: (err: any) => {
        console.error('Failed to fetch patients', err);
        this.filteredPatients = []; // Clear the list on error
      }
    });
  }

  createPatient(): void {
    this.router.navigate(['/patient/create']); // Adjust this route as needed
  }

  onFilterChanged(filter: { medicalRecordNumber: string, name: string, email: string, dateOfBirth: string }): void {
    this.patientService.filterPatients(filter.medicalRecordNumber, filter.name, filter.dateOfBirth, filter.email).subscribe({
      next: (data: PatientsView[]) => {
        this.filteredPatients = data,
          this.totalPages = Math.ceil(this.filteredPatients.length / this.itemsPerPage);
        this.currentPage = 1;
        this.updatePagination();
      },
      error: (err: any) => {
        console.error('Failed to filter patients', err);
        this.filteredPatients = [];
      }
    });
  }

  updatePagination() {
    const startIndex = (this.currentPage - 1) * this.itemsPerPage;
    this.paginatedPatients = this.filteredPatients.slice(startIndex, startIndex + this.itemsPerPage);
  }

  nextPage() {
    if (this.currentPage < this.totalPages) {
      this.currentPage++;
      this.updatePagination();
    }
  }

  previousPage() {
    if (this.currentPage > 1) {
      this.currentPage--;
      this.updatePagination();
    }
  }

  seeDetails(patient: PatientsView): void {
    // Navigate to the patient details page
    this.router.navigate(['/patient/details', patient.medicalRecordNumber]);
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
