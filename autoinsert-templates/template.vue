<template>
  <div class="`(let* ((path-segments (split-string (buffer-file-name) "/")) (vue-app-idx (1+ (-find-index (lambda (s) (string-equal s "vue")) path-segments))) (vue-app (nth vue-app-idx path-segments))) (and vue-app (concat (camel-case (concat "vue-" vue-app)) "-" (camel-case (f-base (buffer-file-name))))))`">
  </div>
</template>

<script lang="ts">
  import Vue from "vue";
  import Component from "vue-class-component";

  @Component({
    $1
  })
  export default class `(pascal-case (f-base (buffer-file-name)))` extends Vue {
    $0
  }
</script>

<style lang="postcss" scoped>
</style>
