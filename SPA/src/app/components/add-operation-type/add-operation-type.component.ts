import { Component } from '@angular/core';

@Component({
  selector: 'app-add-operation-type',
  standalone: true,
  imports: [],
  templateUrl: './add-operation-type.component.html',
  styleUrl: './add-operation-type.component.css'
})
export class AddOperationTypeComponent {
  newOperationType: string = '';

  addOperationType() {
    // Aqui você pode chamar um serviço para adicionar a operação
    console.log('Adding operation type:', this.newOperationType);
  }
}
