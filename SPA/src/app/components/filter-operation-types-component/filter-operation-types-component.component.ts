import { Component, EventEmitter, Output } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-filter-operation-types-component',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './filter-operation-types-component.component.html',
  styleUrls: ['./filter-operation-types-component.component.css']
})
export class FilterOperationTypesComponent {
  filter = {
    name: '',
    status: '',
    specialization: ''
  };

  @Output() filterChanged = new EventEmitter<{ name: string; status: string; specialization: string }>();

  applyFilter(): void {
    this.filterChanged.emit(this.filter);
  }

  onSearch(event: Event): void {
    event.preventDefault();
    this.applyFilter();
  }
}