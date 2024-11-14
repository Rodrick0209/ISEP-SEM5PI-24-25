import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { OperationRequestService } from '../../services/operationRequestService';
import { MessageService } from '../../services/message.service';
import { CommonModule } from '@angular/common';
import { MarkXComponent } from '../template/mark-x/mark-x.component';

@Component({
  selector: 'app-delete-operation-requests',
  standalone: true,
  imports: [CommonModule, MarkXComponent],
  templateUrl: './delete-operation-requests.component.html',
  styleUrl: './delete-operation-requests.component.css'
})
export class DeleteOperationRequestsComponent {

    operationRequestId: string | null = null;
    errorMessage: string = ''; 

    constructor(private route: ActivatedRoute, private operationRequestService: OperationRequestService, private messageService: MessageService, private router: Router) { }


    ngOnInit(): void {
        this.operationRequestId = this.route.snapshot.paramMap.get('id');
    }

    isConfirmed = false; // Track if the "X" mark has been clicked




    onMarkClick(): void {
        this.isConfirmed = !this.isConfirmed;
    }

    onDelete(): void {
        if (this.isConfirmed) {
          if(this.operationRequestId){
            this.operationRequestService.deleteOperationRequest(this.operationRequestId).subscribe({
                next: () => {
                    this.messageService.setMessage(`Operation request successfully deleted!`); // Set the success message
                    this.router.navigate(['/operationRequests']); // Redirect to the operation requests list
                },
                error: (err: any) => {
                    console.error('Failed to delete operation request', err);
                    this.messageService.setMessage('Failed to delete operation request');
                }
            });
          }
        } else {
            this.messageService.setMessage('Please confirm the deletion');
        }
    }

    onCancel(): void {
        this.router.navigate(['/operationRequests']);
    }

}
