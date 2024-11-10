import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { OperationRequest, OperationRequestService } from '../../services/operationRequestService';



@Component({
  selector: 'app-list-operation-requests',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './list-operation-requests.component.html',
  styleUrl: './list-operation-requests.component.css'
})
export class ListOperationRequestsComponent implements OnInit {
  operationRequests: OperationRequest[] = [];
  errorMessage: string | null = null;

  constructor(private operationRequestService: OperationRequestService, private router: Router) { }

  ngOnInit(): void {  
    this.operationRequestService.getOperationRequests().subscribe({
      next: (data : OperationRequest[]) =>{
          this.operationRequests = data;
      },

      error: (err: any) => {  
        console.error('Failed to fetch operation requests', err);
        this.operationRequests = []; // Clear the list on error
        this.errorMessage = 'An error occurred while fetching operation requests: ' + err.error.message;
      }

    });


  }

  deleteOpRequest(operationRequest: OperationRequest): void {
    this.router.navigate(['/operationRequests/delete',operationRequest.id]); // Adjust this route as needed
  }








}
