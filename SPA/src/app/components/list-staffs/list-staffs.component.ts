import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { FilterStaffsComponent } from '../filter-staffs/filter-staffs.component';
import { MessageService } from '../../services/message.service';
import { StaffsView } from '../../models/staff';
import { StaffService } from '../../services/staff.service';

@Component({
  selector: 'app-list-staffs',
  standalone: true,
  imports: [CommonModule, FilterStaffsComponent],
  templateUrl: './list-staffs.component.html',
  styleUrl: './list-staffs.component.css'
})
export class ListStaffsComponent {
  successMessage: string | null = null;
  errorMessage: string | null = null;

  
  staffs: StaffsView[] = [];
  filteredStaffs: StaffsView[] = [];
  paginatedStaffs: StaffsView[] = [];

  currentPage: number = 1;
  itemsPerPage: number = 10;
  totalPages: number = 0;

  constructor(private staffService : StaffService, private router: Router, private messageService: MessageService) { }

  ngOnInit(): void {
    const initialFilter = { fullName: '', licenseNumber: '', phoneNumber: '', email: '', specializationId: '' };
    this.onFilterChanged(initialFilter);
  }

  createStaff(): void {
    this.router.navigate(['/staff/create']); // Adjust this route as needed
  }

  onFilterChanged(filter: { fullName: string, licenseNumber: string, phoneNumber : string, email: string, specializationId: string }): void {
    this.staffService.filterStaffs(filter.fullName, filter.licenseNumber, filter.phoneNumber, filter.email, filter.specializationId).subscribe({
      next: (data: StaffsView[]) => {
        console.log('Dados filtrados:', data); // Verifique o formato
        this.filteredStaffs = data,
          this.totalPages = Math.ceil(this.filteredStaffs.length / this.itemsPerPage);
        this.currentPage = 1;
        this.updatePagination();
      },
      error: (err: any) => {
        console.error('Failed to filter staffs', err);
        this.filteredStaffs = [];
      }
    });
  }

  updatePagination() {
    const startIndex = (this.currentPage - 1) * this.itemsPerPage;
    this.paginatedStaffs = this.filteredStaffs.slice(startIndex, startIndex + this.itemsPerPage);
    console.log('Paginated staffs on page', this.currentPage, ':', this.paginatedStaffs);
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

  seeDetails(staff: StaffsView): void {
    // Navigate to the patient details page
    this.router.navigate(['/staff/details', staff.id]);
    console.log('Details of staff:', staff);
  }

  editStaff(staff: StaffsView): void {
    // Navigate to the edit patient page
    this.router.navigate(['/staff/edit', staff.id]);
    console.log('Editing staff:', staff);
  }

  deleteStaff(staff: StaffsView): void {
    // Implement the delete patient functionality
    this.router.navigate(['/staff/delete', staff.id]);
    console.log('Deleting staff:', staff);
  }

}
