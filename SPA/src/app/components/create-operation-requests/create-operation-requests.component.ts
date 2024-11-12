import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { MessageService } from '../../services/message.service';
import { OperationRequestService } from '../../services/operationRequestService';
import { Router } from '@angular/router';

@Component({
  selector: 'app-create-operation-requests',
  standalone: true,
  imports: [FormsModule,CommonModule],
  templateUrl: './create-operation-requests.component.html',
  styleUrl: './create-operation-requests.component.css'
})
export class CreateOperationRequestsComponent {
  
  
  submitForm = {
      deadLineDate: '',
      priority: '',
      patientId: '',
      operationTypeId: '',
      doctorThatWillPerformId: '' 
  }

  showConfirmation: boolean = false;
  errorMessage: string = '';

  constructor(private router: Router, private operationRequestService: OperationRequestService, private messageService: MessageService) { }

  confirmSubmission(): void {
    this.showConfirmation = true; // Show confirmation modal
  }


  closeConfirmationModal(): void {
    this.showConfirmation = false; // Close the confirmation modal
  }


  onSubmit(operationRequestForm: any): void {
    this.showConfirmation = false;
    if (operationRequestForm.valid) {
      const operationRequestData = {
        ...this.submitForm    
        };
      console.log('DATE = '+operationRequestData.deadLineDate);
      this.operationRequestService.createOperationRequest(
        operationRequestData.deadLineDate,
        operationRequestData.priority,
        operationRequestData.patientId,
        operationRequestData.operationTypeId,
        operationRequestData.doctorThatWillPerformId
      ).subscribe({
        next: () => {
          this.router.navigate(['/operationRequests']);
        },
        error: (err: any) => {
          console.error('Failed to create operation request', err);
          this.errorMessage = 'An error occurred while creating the operation request: ' + err.error.message;
        }
      });
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/operationRequests']);
  }
}
