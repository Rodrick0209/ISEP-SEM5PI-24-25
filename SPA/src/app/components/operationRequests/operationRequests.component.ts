import { Component } from '@angular/core';
import { ListOperationRequestsComponent} from '../list-operation-requests/list-operation-requests.component';

@Component({
  selector: 'app-operationRequests',
  standalone: true,
  imports: [ListOperationRequestsComponent],
  templateUrl: './operationRequests.component.html',
  styleUrl: './operationRequests.component.css'
})
export class OperationRequestsComponent {
  isAddingOperationRequest = false; // Flag to control form visibility





toggleAddOperationRequest() {
  this.isAddingOperationRequest = !this.isAddingOperationRequest; // Toggle the visibility
}


  onFormClosed(): void { 
    this.isAddingOperationRequest = false;
    }

};