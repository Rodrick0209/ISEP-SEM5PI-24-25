import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';
import { CommonModule } from '@angular/common';
import { MarkXComponent } from '../template/mark-x/mark-x.component';

@Component({
  selector: 'app-delete-staff',
  standalone: true,
  imports: [CommonModule, MarkXComponent],
  templateUrl: './delete-staff.component.html',
  styleUrl: './delete-staff.component.css'
})
export class DeleteStaffComponent implements OnInit {
  id : string | null = null;
  errorMessage: string = '';

  constructor(private route: ActivatedRoute, private staffService: StaffService, private messageService: MessageService, private router: Router) { }
  
  ngOnInit(): void {
    this.id = this.route.snapshot.paramMap.get('id');
  }

  isConfirmed = false; // Track if the "X" mark has been clicked

  // Toggle the confirmation mark state
  onMarkClick(): void {
    this.isConfirmed = !this.isConfirmed;
  }

  // Method triggered on delete confirmation
  onDelete(): void {
    if (this.isConfirmed) {
      if (this.id) {
        this.staffService.deleteStaff(this.id).subscribe({
          next: () => {
            this.messageService.setMessage(`Staff  ${this.id} successfully deactivated!`); // Set the success message
            this.router.navigate(['/staffs']); // Redirect to the patients list
          },
          error: (err: any) => {
            console.error('Failed to deactivate staff', err);
            this.errorMessage = 'Failed to deactivate staff';
          }
        });
      }
    } else {
      this.errorMessage = 'Please confirm the deactivation';
    }
  }

  onCancel(): void {
    this.router.navigate(['/staffs']);
  }
}
