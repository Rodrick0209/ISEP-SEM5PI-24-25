import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { OperationRequest, OperationRequestService } from '../../services/operationRequestService';
import { FilterOperationRequestsComponent } from '../filter-operation-requests/filter-operation-requests.component';


@Component({
  selector: 'app-list-operation-requests',
  standalone: true,
  imports: [CommonModule, FilterOperationRequestsComponent],
  templateUrl: './list-operation-requests.component.html',
  styleUrl: './list-operation-requests.component.css'
})
export class ListOperationRequestsComponent implements OnInit {
  operationRequests: OperationRequest[] = [];
  errorMessage: string | null = null;
  filteredOperationRequests: OperationRequest[] = [];
  isLoading = false;


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

  //operationType: '',
  //patientName: '',
  //medicalRecordNumber: '',	
  //startDate: '',
  //endDate: '',

  onFilterChanged(filter: { operationType: string; patientName: string, medicalRecordNumber: string, startDate: string, endDate: string}) : void {
    this.isLoading = true;  
    this.operationRequests = [];  // Limpar as operation requests antes de carregar novas
    this.operationRequestService.filterOperationRequests(filter).subscribe({
        next: (data) => this.operationRequests = data,
        complete: () => this.isLoading = false,
        error: (err) => {
          console.error('Failed to filter operation requests', err);
          this.isLoading = false;
          this.filteredOperationRequests = []; // Clear the list on error
          
        }
      });
  }




  updateOpRequest(operationRequest: OperationRequest): void {
    this.router.navigate(['/operationRequests/edit',operationRequest.id]); // Adjust this route as needed
  } 







}
