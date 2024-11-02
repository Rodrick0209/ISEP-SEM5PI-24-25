import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { OperationTypesService, OperationType } from '../../services/operation-type.service';
import { FilterOperationTypesComponent } from '../filter-operation-types-component/filter-operation-types-component.component';

@Component({
  selector: 'app-list-operation-types',
  standalone: true,
  imports: [CommonModule, FilterOperationTypesComponent],
  templateUrl: './list-operation-types.component.html',
  styleUrls: ['./list-operation-types.component.css']
})
export class ListOperationTypesComponent implements OnInit {
  operationTypes: OperationType[] = [];
  filteredOperationTypes: OperationType[] = [];

  constructor(private operationTypesService: OperationTypesService) { }

  ngOnInit(): void {
    this.operationTypesService.getOperationTypes().subscribe({
      next: (data) => {
        this.operationTypes = data;
        this.filteredOperationTypes = data;
      },
      error: (err) => {
        console.error('Failed to fetch operation types', err);
        this.filteredOperationTypes = []; // Clear the list on error
      }
    });
  }

  onFilterChanged(filter: { name: string; status: string; specialization: string }): void {
    this.operationTypesService.filterOperationTypes(filter.name, filter.status, filter.specialization).subscribe({
      next: (data) => this.filteredOperationTypes = data,
      error: (err) => {
        console.error('Failed to filter operation types', err);
        this.filteredOperationTypes = []; // Clear the list on error
      }
    });
  }

  editOperationType(operationType: OperationType): void {
    // Implement your edit logic here
    console.log('Editing operation type:', operationType);
  }

  deactivateOperationType(operationType: OperationType): void {
    this.operationTypesService.deactivateOperationType(operationType.id).subscribe({
      next: () => {
        this.ngOnInit();
      },
      error: (err) => {
        if (err.status === 401) {
          alert('Authentication token is invalid or expired. Please log in again.');
          // Redirect to login page or handle re-authentication
        } else {
          console.error('Failed to deactivate operation type', err);
        }
      }
    });
  }

  isActive(operationType: OperationType): boolean {
    return operationType.status=='active';
  }
}