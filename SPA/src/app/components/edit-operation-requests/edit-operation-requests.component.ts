import { Component, OnInit } from '@angular/core';
import { ActivatedRoute,Router } from '@angular/router';
import { OperationRequestService } from '../../services/operationRequestService';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-edit-operation-requests',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './edit-operation-requests.component.html',
  styleUrl: './edit-operation-requests.component.css'
})
export class EditOperationRequestsComponent implements OnInit {
  submitForm = {
    operationRequestId: '',
    deadline: '',
    priority: '',
  }

  operationRequestId: string | null = null;
  errorMessage: string = '';
  showConfirmation: boolean = false;


  constructor(private router: Router, private operationRequestService: OperationRequestService, private route: ActivatedRoute, private messageService: MessageService) { }

  ngOnInit(): void {
    this.operationRequestId = this.route.snapshot.paramMap.get('id');
  }

  confirmSubmission(): void {
    this.showConfirmation = true;
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false;
  }

  onSubmit(operationRequestForm: any): void {
    this.showConfirmation = false;
    if(operationRequestForm.valid){
        const operationRequestData : any = {};
    if (this.submitForm.deadline)
    {
        operationRequestData.deadline = this.submitForm.deadline;
    }    
    if (this.submitForm.priority)
    {
        operationRequestData.priority = this.submitForm.priority;
    }  

    if (this.operationRequestId) {
        operationRequestData.Id = this.operationRequestId;  
        this.operationRequestService.editOperationRequest(
            operationRequestData
          ).subscribe(
            () => {
              this.messageService.setMessage('Operation Request has been updated successfully');
              this.router.navigate(['/operationRequests']);
            },
            (error) => {
              console.error('Erro ao atualizar Operation Request:', error);  // Exibe o erro completo no console
              console.error('Detalhes do erro:', error.error);  // Exibe a resposta do backend, se disponível
              this.errorMessage = 'Failed to update Operation Request';
            }
            );
        } else {
          this.errorMessage = 'Operation Request ID is missing';
        }
      }
    }


    onCancel(): void {
      this.showConfirmation = false;  
      this.router.navigate(['/operationRequests']);
    }

  }


