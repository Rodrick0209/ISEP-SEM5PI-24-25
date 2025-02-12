import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { FilterPatientsComponent } from '../filter-patients/filter-patients.component';
import { MessageService } from '../../services/message.service';
import { PatientsView } from '../../models/patient';
import { PatientService } from '../../services/patient.service';
import { AuthService } from '../../services/auth.service';

@Component({
  selector: 'app-list-patients',
  standalone: true,
  imports: [CommonModule, FilterPatientsComponent],
  templateUrl: './list-patients.component.html',
  styleUrl: './list-patients.component.css'
})
export class ListPatientsComponent implements OnInit {
  successMessage: string | null = null;
  errorMessage: string | null = null;

  patients: PatientsView[] = [];
  filteredPatients: PatientsView[] = [];
  paginatedPatients: PatientsView[] = [];

  currentPage: number = 1;
  itemsPerPage: number = 10;
  totalPages: number = 0;

  constructor(private patientService: PatientService, private router: Router, private messageService: MessageService, private authService: AuthService) { }

  ngOnInit(): void {
    this.successMessage = this.messageService.getMessage();
    this.onFilterChanged({ medicalRecordNumber: '', name: '', email: '', dateOfBirth: '' });
  }

  extractRole(): any {
    return this.authService.extractRoleFromToken();
  }

  isAdmin(): boolean {
    const role = this.extractRole();
    return role === 'admin';
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
    this.router.navigate(['/patient/edit', patient.medicalRecordNumber]);
    console.log('Editing patient:', patient);
  }

  deletePatient(patient: PatientsView): void {
    // Implement the delete patient functionality
    this.router.navigate(['/patient/delete', patient.medicalRecordNumber]);
    console.log('Deleting patient:', patient);
  }

}
