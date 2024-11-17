import { Component } from '@angular/core';
import { NavigationExtras, Router } from '@angular/router';
import { StaffService } from '../../services/staff.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { MessageService } from '../../services/message.service';
import { Category, StaffStatus } from '../../models/staff';

@Component({
  selector: 'app-create-staff',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './create-staff.component.html',
  styleUrls: ['./create-staff.component.css'] // Ensure this is plural
})
export class CreateStaffComponent {

  Category = Category;
  categories: string[] = Object.values(Category); 

  
  
  submitForm = {
    fullName: '',
    licenseNumber: '',
    specialization: '',
    email: '',
    phoneNumber: '',
    category: ''
  }

  showConfirmation: boolean = false;
  errorMessage: string = '';

  constructor(private router: Router, private staffService: StaffService, private messageService: MessageService) { }

  confirmSubmission(): void {
    this.showConfirmation = true; // Show confirmation modal
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false; // Close the confirmation modal
  }

  onSubmit(staffForm: any): void {
    this.showConfirmation = false;
    if (staffForm.valid) {
      const staffData = {
        ...this.submitForm
      };

      this.staffService.createStaff(
        staffData.fullName,
        staffData.licenseNumber,
        staffData.specialization,
        staffData.email,
        staffData.phoneNumber,
        staffData.category
      ).subscribe({
        next: (data: any) => {
          console.log('Staff created successfully', data);
          this.messageService.setMessage(`Staff ${staffData.fullName} successfully created!`);
          this.router.navigate(['/staffs']);
        },
        error: (err: any) => {
          console.log('Failed to create staff', err);
          this.errorMessage = 'Failed to create staff: ' + (err.error?.message || 'Unknown error');
        }
      });
    } else {
      console.log('Form is invalid');
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/staffs']);
  }
}
