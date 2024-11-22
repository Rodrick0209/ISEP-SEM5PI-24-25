import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, NavigationExtras, Router } from '@angular/router';
import { StaffService } from '../../services/staff.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-edit-staff',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './edit-staff.component.html',
  styleUrl: './edit-staff.component.css'
})
export class EditStaffComponent implements OnInit {
  submitForm = {
    fullName: '',
    licenseNumber: '',
    phoneNumber: '',
    email: '',
    specializationId: ''
  }

  id: string | null = null;
  errorMessage: string = '';
  showConfirmation: boolean = false;
  
  constructor(private router: Router, private staffService: StaffService, private route: ActivatedRoute, private messageService: MessageService) { }

  ngOnInit(): void {
    this.id = this.route.snapshot.paramMap.get('id');
  }

  confirmSubmission(): void {
    this.showConfirmation = true;
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false;
  }

  onSubmit(staffForm: any): void {
    this.showConfirmation = false;
    if (staffForm.valid) {
      const staffData = {
        ...this.submitForm,
      };

      if (this.id) {
        this.staffService.editStaff(
          this.id,
          staffData.fullName,
          staffData.licenseNumber,
          staffData.phoneNumber,
          staffData.email,
          staffData.specializationId
        ).subscribe(
          (response) => {
            console.log("Staff edited successfully", response);
            this.messageService.setMessage(`Staff  ${this.id} successfully edited!`);
            this.router.navigate(['/staffs']);
          },
          (error) => {
            this.errorMessage = error.error.message;
            console.error("Failed to edit staff", error);
          }
        );
      } else {
        this.errorMessage = 'Id is missing.';
      }
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/staffs']);
  }
}
