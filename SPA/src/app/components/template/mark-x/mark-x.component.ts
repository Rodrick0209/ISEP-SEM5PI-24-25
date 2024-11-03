import { CommonModule } from '@angular/common';
import { Component, EventEmitter, Input, Output, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

@Component({
  selector: 'app-mark-x',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './mark-x.component.html',
  styleUrl: './mark-x.component.css',
})
export class MarkXComponent {
  showMark: boolean = false; // Controls visibility of the "X"
  
  @Input() markText: string = 'I confirm'; // Accept markText from the parent
  @Output() markClicked = new EventEmitter<void>();

  onMarkClick(): void {
    this.markClicked.emit(); // Emit the event to the parent
    this.showMark = !this.showMark; // Toggle the visibility of the mark
  }
}
