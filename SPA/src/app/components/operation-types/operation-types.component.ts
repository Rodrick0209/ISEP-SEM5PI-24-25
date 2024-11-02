import { Component } from '@angular/core';
import { ListOperationTypesComponent } from '../list-operation-types/list-operation-types.component';
import { FormsModule } from '@angular/forms'; // Import FormsModule
import { AddOperationTypeComponent } from '../add-operation-type/add-operation-type.component';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-operation-types',
  standalone: true,
  imports: [ListOperationTypesComponent, AddOperationTypeComponent, CommonModule],
  templateUrl: './operation-types.component.html',
  styleUrl: './operation-types.component.css'
})
export class OperationTypeComponent {
  isAddingOperationType = false; // Flag to control form visibility
  operationType = {
    name: '',
    // Other properties like status, specialization, etc., if needed
  };

  toggleAddOperationType() {
    this.isAddingOperationType = !this.isAddingOperationType; // Toggle the visibility
  }

  onFormClosed(): void {
    this.isAddingOperationType = false;
  }
}
