import { Component } from '@angular/core';
import { ListOperationTypesComponent } from '../list-operation-types/list-operation-types.component';
import { AddOperationTypeComponent } from '../add-operation-type/add-operation-type.component';

@Component({
  selector: 'app-operation-types',
  standalone: true,
  imports: [ListOperationTypesComponent, AddOperationTypeComponent],
  templateUrl: './operation-types.component.html',
  styleUrl: './operation-types.component.css'
})
export class OperationTypeComponent {
  operationTypes: string[] = [];

  onOperationAdded(newOperation: string) {
    this.operationTypes.push(newOperation); // Adiciona nova operação à lista
  }
}
