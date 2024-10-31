import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { OperationTypesService, OperationType } from '../../services/operation-type.service';


@Component({
  selector: 'app-list-operation-types',
  standalone: true,
  imports: [CommonModule], // Import CommonModule here
  templateUrl: './list-operation-types.component.html',
  styleUrls: ['./list-operation-types.component.css'] // Corrected from styleUrl to styleUrls
})
export class ListOperationTypesComponent implements OnInit {
  operationTypes: OperationType[] = [];

  constructor(private operationTypesService: OperationTypesService) {}

  ngOnInit(): void {
    this.operationTypesService.getOperationTypes().subscribe({
      next: (data) => this.operationTypes = data,
      error: (err) => console.error('Failed to fetch operation types', err)
    });
  }
  

  

}